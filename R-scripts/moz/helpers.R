# No evaluation of XLSForm expressions: constraints/relevance remain in the dictionary.
blank <- function(x) is.na(x) | trimws(as.character(x)) == ''
norm <- function(x) {
  x[is.na(x)] <- ''
  trimws(gsub('[[:space:]]+', ' ', enc2utf8(as.character(x))))
}
read_sheet <- function(path, sheet) {
  as.data.frame(readxl::read_xlsx(path, sheet = sheet, col_types = 'text',
                                .name_repair = 'minimal'), check.names = FALSE)
}
form_dictionary <- function(tool) {
  survey <- read_sheet(tool, 'survey')
  choices <- read_sheet(tool, 'choices')
  scope <- 'Submissions'; stack <- character(); scopes <- character(nrow(survey))
  for (i in seq_len(nrow(survey))) {
    t <- gsub('_', ' ', survey$type[i])
    if (!is.na(t) && t == 'begin repeat') {
      stack <- c(stack, survey$name[i]); scope <- tail(stack, 1)
    }
    scopes[i] <- scope
    if (!is.na(t) && t == 'end repeat') {
      stack <- head(stack, -1); scope <- if (length(stack)) tail(stack, 1) else 'Submissions'
    }
  }
  survey$scope <- scopes
  list(survey = survey, choices = choices)
}
# Unique normalized names or exact labels only. Duplicate labels are NOT resolved by order.
map_headers <- function(headers, scope, survey, choices) {
  s <- survey[survey$scope == scope & !blank(survey$name), , drop = FALSE]
  candidates <- data.frame(header = character(), target = character())
  add <- function(h, target) {
    h <- h[!blank(h)]
    if (length(h)) candidates <<- rbind(candidates, data.frame(header = norm(h), target = target))
  }
  for (i in seq_len(nrow(s))) {
    r <- s[i, ]; labels <- unlist(r[intersect(c('label::english', 'label::portuguese'), names(r))], use.names = FALSE)
    add(c(r$name, labels, substr(labels, 1, 255)), r$name)
    if (!is.na(r$type) && startsWith(r$type, 'select_multiple ')) {
      ch <- choices[choices$list_name == sub('^select_multiple +', '', r$type) & !is.na(choices$list_name), , drop = FALSE]
      for (j in seq_len(nrow(ch))) {
        opts <- unlist(ch[j, intersect(c('name', 'label::english', 'label::portuguese'), names(ch))], use.names = FALSE)
        opts <- opts[!blank(opts)]
        for (lab in c(r$name, labels[!blank(labels)], substr(labels[!blank(labels)], 1, 255)))
          add(paste0(lab, '/', opts), paste0(r$name, '/', ch$name[j]))
      }
    }
  }
  out <- data.frame(position = seq_along(headers), original = headers,
                    variable = headers, status = 'unmapped', stringsAsFactors = FALSE)
  metadata <- c('Id', 'uuid', '_id', '_uuid', '_index', '_parent_index', '_parent_table_name',
                'ADM1_PCODE', 'ADM2_PCODE', 'ADM3_PCODE')
  for (i in seq_along(headers)) {
    if (headers[i] %in% metadata) {out$status[i] <- 'metadata'; next}
    hit <- unique(candidates$target[candidates$header == norm(headers[i])])
    if (length(hit) == 1) {out$variable[i] <- hit; out$status[i] <- 'mapped'}
    if (length(hit) > 1) out$status[i] <- 'ambiguous'
  }
  collision <- duplicated(out$variable) | duplicated(out$variable, fromLast = TRUE)
  out$status[collision] <- 'collision'
  out$variable[collision] <- out$original[collision]
  out$variable <- make.unique(out$variable, sep = '__duplicate_')
  out
}
# Decode full labels separated by spaces, as in this export ("20 litres 25 litres").
# Try all segmentations and accept exactly one sequence. Commas inside labels are preserved.
decode_multiple <- function(value, ch) {
  if (blank(value)) return(NA_character_)
  aliases <- do.call(rbind, lapply(intersect(c('name', 'label::english', 'label::portuguese'), names(ch)), function(k)
    data.frame(alias = norm(ch[[k]]), code = ch$name, stringsAsFactors = FALSE)))
  aliases <- unique(aliases[!blank(aliases$alias), ])
  value <- norm(value)
  walk <- function(rest, codes = character()) {
    if (!nzchar(rest)) return(list(paste(codes, collapse = ';')))
    result <- list()
    for (i in seq_len(nrow(aliases))) {
      a <- aliases$alias[i]
      if (rest == a) result <- c(result, list(paste(c(codes, aliases$code[i]), collapse = ';')))
      else if (startsWith(rest, paste0(a, ' ')) || startsWith(rest, paste0(a, ';'))) {
        tail <- trimws(sub('^;[ ]*', '', substring(rest, nchar(a) + 1)))
        result <- c(result, walk(tail, c(codes, aliases$code[i])))
      }
      result <- as.list(unique(unlist(result)))
      if (length(result) > 1) break
    }
    result
  }
  result <- unique(unlist(walk(value)))
  if (length(result) == 1) result else NA_character_
}
normalize_choices <- function(df, scope, dictionary) {
  s <- dictionary$survey
  s <- s[s$scope == scope & !is.na(s$type) & grepl('^select_(one|multiple) ', s$type), , drop = FALSE]
  problems <- list()
  for (i in seq_len(nrow(s))) {
    q <- s$name[i]; if (!q %in% names(df)) next
    ch <- dictionary$choices
    ch <- ch[!is.na(ch$list_name) & ch$list_name == sub('^select_(one|multiple) +', '', s$type[i]), , drop = FALSE]
    original <- df[[q]]
    converted <- rep(NA_character_, length(original))
    # Hierarchical administrative choices must be resolved in their parent context.
    filter <- s$choice_filter[i]
    refs <- if (!is.na(filter)) regmatches(filter, gregexpr('\\$\\{[^}]+\\}', filter))[[1]] else character()
    refs <- gsub('^\\$\\{|\\}$', '', refs)
    filters <- if (!is.na(filter)) regmatches(filter, gregexpr('filter_[A-Za-z0-9_]+', filter))[[1]] else character()
    parent_values <- rep('', length(original))
    contextual <- length(refs) == 1 && length(filters) == 1 && refs %in% names(df) && filters %in% names(ch)
    if (contextual) parent_values <- df[[refs]]
    group_key <- interaction(original, parent_values, drop = TRUE, lex.order = TRUE)
    active <- which(!blank(original) & !is.na(group_key))
    groups <- split(active, group_key[active])
    for (rows in groups) {
      value <- original[rows[1]]; opts <- ch
      if (contextual) {
        parent <- parent_values[rows[1]]
        if (!blank(parent)) opts <- opts[!is.na(opts[[filters]]) & opts[[filters]] == parent, , drop = FALSE]
      }
      if (startsWith(s$type[i], 'select_multiple ')) converted[rows] <- decode_multiple(value, opts)
      else {
        hit <- unique(opts$name[Reduce(`|`, lapply(intersect(c('name','label::english','label::portuguese'), names(opts)), function(c) norm(opts[[c]]) == norm(value)))])
        if (length(hit) == 1) converted[rows] <- hit
      }
    }
    bad <- which(!blank(original) & is.na(converted))
    if (length(bad)) problems[[q]] <- data.frame(row = bad, variable = q, issue = 'choice_not_uniquely_mapped')
    # Preserve unresolved answers verbatim, with an explicit review flag.
    converted[bad] <- original[bad]
    df[[q]] <- converted
  }
  list(data = df, problems = problems)
}
# Stable local keys are valid only when the Kobo sheet index is present and unique.
record_keys <- function(df) {
  if (!'_index' %in% names(df) || any(blank(df$`_index`)) || anyDuplicated(df$`_index`))
    stop('Missing or duplicate _index. Export cannot be used for safe correction keys.')
  as.character(df$`_index`)
}
apply_feedback <- function(data, feedback) {
  required <- c('dataset','record_key','variable','old_value','new_value','action','approved','reviewer','reason')
  if (!all(required %in% names(feedback))) stop('Invalid feedback columns.')
  feedback <- feedback[tolower(feedback$approved) == 'yes' & !is.na(feedback$approved), , drop = FALSE]
  if (anyDuplicated(feedback[c('dataset','record_key','variable')])) stop('Conflicting feedback targets.')
  for (i in seq_len(nrow(feedback))) {
    f <- feedback[i, ]
    if (blank(f$reviewer) || blank(f$reason)) stop('Approved correction requires reviewer and reason.')
    if (!f$action %in% c('replace','set_missing')) stop('Supported actions: replace, set_missing.')
    if (!f$dataset %in% names(data)) stop('Unknown feedback dataset: ', f$dataset)
    d <- data[[f$dataset]]; j <- match(f$record_key, d$.record_key)
    if (is.na(j) || !f$variable %in% names(d)) stop('Feedback target missing.')
    if (f$variable %in% c('Id','uuid','_index','_parent_index','_parent_table_name','.record_key','.household_uuid')) stop('Cannot edit linkage identifiers.')
    old <- d[[f$variable]][j]
    if (!identical(if (blank(old)) '' else old, if (blank(f$old_value)) '' else f$old_value)) stop('Stale old_value for feedback row ', i)
    if (f$action == 'replace' && blank(f$new_value)) stop('Use set_missing for an empty replacement.')
    d[[f$variable]][j] <- if (f$action == 'set_missing') NA_character_ else f$new_value
    data[[f$dataset]] <- d
  }
  list(data = data, applied = feedback)
}
