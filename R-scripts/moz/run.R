# Run from the repository root in RStudio: source('R-scripts/moz/run.R')
# CLI: Rscript R-scripts/moz/run.R [tool.xlsx] [data.xlsx]
for (pkg in c('readxl', 'openxlsx'))
  if (!requireNamespace(pkg, quietly = TRUE)) stop('Install package: ', pkg)
if (!file.exists('config/moz.R')) stop('Open MSNA_cleaning_2026.Rproj and run from the project root.')
source('config/moz.R'); source('R-scripts/moz/helpers.R')
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) moz_config$tool <- args[1]
if (length(args) >= 2) moz_config$data <- args[2]
if (!all(file.exists(c(moz_config$tool, moz_config$data)))) stop('Input not found. Edit config/moz.R.')
run_dir <- file.path(moz_config$output, format(Sys.time(), '%Y-%m-%d_%H%M%S'))
if (dir.exists(run_dir)) stop('Run directory already exists.')
dir.create(run_dir, recursive = TRUE)
dictionary <- form_dictionary(moz_config$tool)
sheets <- readxl::excel_sheets(moz_config$data)
if (!all(unname(moz_sheets) %in% sheets)) stop('Expected sheets missing: ', paste(setdiff(moz_sheets, sheets), collapse = ', '))
unknown <- setdiff(sheets, unname(moz_sheets))
if (length(unknown)) stop('Unconfigured sheets (add them to moz_sheets): ', paste(unknown, collapse = ', '))
raw <- mapped <- list(); maps <- list()
for (dataset in names(moz_sheets)) {
  message('Reading ', dataset)
  raw[[dataset]] <- read_sheet(moz_config$data, moz_sheets[[dataset]])
  mp <- map_headers(names(raw[[dataset]]), dataset, dictionary$survey, dictionary$choices)
  mp$dataset <- dataset; maps[[dataset]] <- mp
  d <- raw[[dataset]]; names(d) <- mp$variable
  d$.record_key <- record_keys(d)
  mapped[[dataset]] <- d
}
saveRDS(raw, file.path(run_dir, 'raw_snapshot.rds'))
column_map <- do.call(rbind, maps)
write.csv(column_map, file.path(run_dir, 'column_mapping.csv'), row.names = FALSE, na = '')
# Keep explicit states. Feedback applies to normalized column names but ORIGINAL answer values.
saveRDS(mapped, file.path(run_dir, 'mapped_before_feedback.rds'))
applied <- data.frame()
if (file.exists(moz_config$feedback)) {
  f <- read.csv(moz_config$feedback, colClasses = 'character', check.names = FALSE, na.strings = '', fileEncoding = 'UTF-8-BOM')
  result <- apply_feedback(mapped, f); mapped <- result$data; applied <- result$applied
}
write.csv(applied, file.path(run_dir, 'applied_corrections.csv'), row.names = FALSE, na = '')
flags <- list()
flag <- function(dataset, rows, variable, issue, observed = NULL) {
  rows <- rows[!is.na(rows)]; if (!length(rows)) return(invisible(NULL))
  d <- mapped[[dataset]]
  if (is.null(observed)) observed <- if (variable %in% names(d)) d[[variable]][rows] else rep('', length(rows))
  flags[[length(flags)+1L]] <<- data.frame(dataset = dataset, record_key = d$.record_key[rows],
    variable = variable, issue = issue, observed = as.character(observed), stringsAsFactors = FALSE)
}
main <- mapped$Submissions
if (!all(c('uuid','_index') %in% names(main))) stop('Household linkage fields missing.')
flag('Submissions', which(blank(main$uuid)), 'uuid', 'missing_household_uuid')
flag('Submissions', which(duplicated(main$uuid) | duplicated(main$uuid, fromLast = TRUE)), 'uuid', 'duplicate_household_uuid')
for (dataset in names(mapped)) {
  d <- mapped[[dataset]]
  if (dataset == 'Submissions') d$.household_uuid <- d$uuid else {
    if (!all(c('_parent_index','uuid') %in% names(d))) stop('Repeat linkage fields missing: ', dataset)
    parent <- match(d$`_parent_index`, main$`_index`)
    d$.household_uuid <- main$uuid[parent]
    flag(dataset, which(is.na(parent)), '_parent_index', 'orphan_repeat')
    flag(dataset, which(!is.na(parent) & (blank(d$uuid) | d$uuid != d$.household_uuid)), 'uuid', 'parent_uuid_mismatch')
    if ('Id' %in% names(d) && 'Id' %in% names(main))
      flag(dataset, which(!is.na(parent) & !blank(d$Id) & d$Id != main$Id[parent]), 'Id', 'parent_id_mismatch')
  }
  normalized <- normalize_choices(d, dataset, dictionary)
  mapped[[dataset]] <- normalized$data
  for (p in normalized$problems) flag(dataset, p$row, p$variable[1], p$issue[1])
  d <- mapped[[dataset]]
  s <- dictionary$survey
  nums <- s[s$scope == dataset & s$type %in% c('integer','decimal'), , drop = FALSE]
  for (i in seq_len(nrow(nums))) {
    q <- nums$name[i]; if (!q %in% names(d)) next
    x <- suppressWarnings(as.numeric(d[[q]]))
    flag(dataset, which(!blank(d[[q]]) & (is.na(x) | !is.finite(x))), q, 'not_numeric')
    flag(dataset, which(!is.na(x) & nums$type[i] == 'integer' & x != trunc(x)), q, 'non_integer')
    # Respect explicit negative sentinel codes permitted by the XLSForm.
    con <- nums$constraint[i]
    allowed <- if (!is.na(con)) as.numeric(regmatches(con, gregexpr('-[0-9]+', con))[[1]]) else numeric()
    flag(dataset, which(!is.na(x) & x < 0 & !x %in% allowed), q, 'negative_not_in_form')
    if (q %in% c('resp_age','hoh_age','ind_age','proxy_resp_age'))
      flag(dataset, which(!is.na(x) & !x %in% allowed & (x < 0 | x > 120)), q, 'age_outside_0_120')
    if (grepl('^m32[0-9]+_hh_fcs_|^fsl_rcsi_', q))
      flag(dataset, which(!is.na(x) & (x < 0 | x > 7)), q, 'days_outside_0_7')
  }
}
main <- mapped$Submissions
if ('hh_size' %in% names(main)) {
  counts <- table(mapped$roster$.household_uuid)
  n <- as.integer(counts[match(main$uuid, names(counts))]); n[is.na(n)] <- 0L
  size <- suppressWarnings(as.numeric(main$hh_size))
  flag('Submissions', which(!is.na(size) & size != n), 'hh_size', 'roster_count_mismatch', paste0('hh_size=',size[!is.na(size) & size != n], '; roster=', n[!is.na(size) & size != n]))
}
# Verify sector-person links against the roster using household + person UUID together.
roster <- mapped$roster
if ('person_id' %in% names(roster)) {
  rk <- paste(roster$.household_uuid, roster$person_id, sep = '::')
  flag('roster', which(!blank(roster$person_id) & (duplicated(rk) | duplicated(rk, fromLast = TRUE))), 'person_id', 'duplicate_person_within_household')
  fields <- c(nut_ind='nut_person_id', edu_ind='edu_person_id', health_ind='health_person_id', health_wra='health_wra_person_id')
  for (dataset in names(fields)) {
    d <- mapped[[dataset]]; q <- fields[[dataset]]
    if (!q %in% names(d)) next
    keys <- paste(d$.household_uuid, d[[q]], sep = '::')
    flag(dataset, which(blank(d[[q]]) | !keys %in% rk), q, 'person_missing_from_household_roster')
    flag(dataset, which(!blank(d[[q]]) & (duplicated(keys) | duplicated(keys, fromLast = TRUE))), q, 'duplicate_sector_person')
  }
}
if ('consent' %in% names(main)) flag('Submissions', which(blank(main$consent) | main$consent != 'yes'), 'consent', 'consent_review')
# Candidate datasets retain all records. No automatic exclusions, imputation or winsorisation.
issues <- if (length(flags)) do.call(rbind, flags) else data.frame(dataset=character(),record_key=character(),variable=character(),issue=character(),observed=character())
write.csv(issues, file.path(run_dir, 'review_issues.csv'), row.names = FALSE, na = '')
summary <- if (nrow(issues)) aggregate(list(n = issues$record_key), issues[c('dataset','variable','issue')], length) else data.frame()
write.csv(summary, file.path(run_dir, 'review_summary.csv'), row.names = FALSE)
saveRDS(mapped, file.path(run_dir, 'candidate_data.rds'))
for (dataset in names(mapped)) write.csv(mapped[[dataset]], file.path(run_dir, paste0(dataset,'.csv')), row.names = FALSE, na = '')
template <- data.frame(dataset=character(),record_key=character(),variable=character(),old_value=character(),new_value=character(),action=character(),approved=character(),reviewer=character(),reason=character())
write.csv(template, file.path(run_dir, 'corrections_template.csv'), row.names = FALSE)
wb <- openxlsx::createWorkbook()
for (tab in c('summary','issues','mapping')) {
  openxlsx::addWorksheet(wb, tab)
  value <- switch(tab, summary=summary, issues=issues, mapping=column_map)
  openxlsx::writeData(wb, tab, value, withFilter = ncol(value) > 0)
  openxlsx::freezePane(wb, tab, firstRow = TRUE)
  if (ncol(value)) openxlsx::setColWidths(wb, tab, cols=seq_len(ncol(value)), widths=24)
}
openxlsx::saveWorkbook(wb, file.path(run_dir, 'review_log.xlsx'), overwrite = FALSE)
writeLines(c(capture.output(sessionInfo()), '', paste('Source MD5:', names(tools::md5sum(c(moz_config$tool, moz_config$data))), tools::md5sum(c(moz_config$tool, moz_config$data)))) , file.path(run_dir, 'run_manifest.txt'))
message('Completed. Review issues and mapping before analysis: ', run_dir)
