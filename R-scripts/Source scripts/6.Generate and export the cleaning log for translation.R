cat("\014")
print_step("6. Generating translation / free-text review log")

translation_logs <- purrr::imap(raw_data_list, ~ translation_flag_log(.x, .y, uuid_col = "uuid"))
translation_log <- dplyr::bind_rows(translation_logs) |>
  dplyr::anti_join(cleaning_log_template |> dplyr::select(dataset, uuid, question), by = c("dataset", "uuid", "question"))

free_text_cols <- purrr::imap_dfr(raw_data_list, function(df, nm) {
  if (nrow(df) == 0) return(tibble::tibble())
  tibble::tibble(dataset = nm, variable = names(df), is_character = vapply(df, is.character, logical(1))) |>
    dplyr::filter(is_character, stringr::str_detect(variable, "other|specify|comment|note|reason"))
})

translation_export <- list(
  translation_review_log = translation_log,
  free_text_columns = free_text_cols
)

translation_path <- export_excel_list(translation_export, paste0("Cleaning_log_translation_review_", format(Sys.time(), "%Y-%m-%d_at_%Hh%M"), ".xlsx"), path_cleaning_folder_today)
cat("Exported: ", translation_path, "\n", sep = "")
print_step("Translation/free-text review log exported")
