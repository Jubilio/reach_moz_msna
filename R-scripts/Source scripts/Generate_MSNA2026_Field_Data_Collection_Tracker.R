cat("\014")

# =================================================================================================
# Generate MSNA 2026 Mozambique Field Data Collection Tracker - RMarkdown report
# -------------------------------------------------------------------------------------------------
# This script should be sourced from the master cleaning script after scripts 1-5 have created:
#   - raw_data_list / data_hh_main
#   - combined_log$checked_dataset
#   - combined_log$cleaning_log
#   - cleaning_log_template / cleaning_log
#   - path_cleaning_folder_today
# =================================================================================================

if (exists("print_step", mode = "function")) {
  print_step("Generating MSNA 2026 Mozambique field data collection tracker RMarkdown report")
} else {
  message("Generating MSNA 2026 Mozambique field data collection tracker RMarkdown report")
}

if (!requireNamespace("rmarkdown", quietly = TRUE)) {
  stop("Package 'rmarkdown' is required to render the field data collection tracker.", call. = FALSE)
}

if (!exists("path_cleaning_folder_today")) {
  path_cleaning_folder_today <- file.path(
    "outputs",
    paste0("MSNA2026 Mozambique data cleaning ", format(Sys.Date(), format = "%Y-%b-%d"))
  )
}

if (!dir.exists(path_cleaning_folder_today)) {
  dir.create(path_cleaning_folder_today, recursive = TRUE)
}

tracker_rmd_candidates <- c(
  "R-scripts/Source scripts/msna_field_data_collection_tracker_MSNA2026.Rmd",
  "R-scripts/Source scripts/msna_field_data_collection_tracker.Rmd",
  "R-scripts/msna_field_data_collection_tracker_MSNA2026.Rmd",
  "msna_field_data_collection_tracker_MSNA2026.Rmd",
  "msna_field_data_collection_tracker.Rmd"
)

tracker_rmd_path <- tracker_rmd_candidates[file.exists(tracker_rmd_candidates)][1]

if (is.na(tracker_rmd_path)) {
  stop(
    "Field tracker RMarkdown file was not found. Save the updated Rmd as one of: ",
    paste(tracker_rmd_candidates, collapse = ", "),
    call. = FALSE
  )
}

tracker_output_file <- paste0(
  "MSNA2026_Mozambique_Field_Data_Collection_Tracker_",
  format(Sys.Date(), "%Y_%m_%d"),
  ".html"
)

tracker_output_path <- rmarkdown::render(
  input = tracker_rmd_path,
  output_dir = path_cleaning_folder_today,
  output_file = tracker_output_file,
  envir = globalenv(),
  quiet = FALSE
)

field_tracker_report_path <- tracker_output_path

cat("\n")
cat("Field data collection tracker exported: ", field_tracker_report_path, "\n", sep = "")
cat("\n")

if (exists("green") && is.function(green)) {
  cat(green$bold("Field data collection tracker report completed"))
} else if (requireNamespace("crayon", quietly = TRUE)) {
  cat(crayon::green$bold("Field data collection tracker report completed"))
} else {
  cat("Field data collection tracker report completed")
}
cat("\n")
