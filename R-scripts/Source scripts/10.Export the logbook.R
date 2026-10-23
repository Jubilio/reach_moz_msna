cat("\014")  


if(!dir_exists("outputs/Logbook for submission")){
  dir_create("outputs/Logbook for submission")
}


#Export the final outputs, raw df, clean df, clean df converted, you already have the logbook exported

export_in_reach_format_multiple_sheets(
  file_R_1 = raw_data, sheet_name_1 = "Raw dataset",
  file_R_2 = clean_df, sheet_name_2 = "Clean dataset",
  file_R_3 = clean_df_converted, sheet_name_3 = "Clean dataset converted",
  # file_R_4 = variable_tracker, sheet_name_4 = "Variable tracker",
  file_R_5 = final_cleaning_log_no_dl, sheet_name_5 = "Cleaning log",
  file_R_6 = deletion_log, sheet_name_6 = "Deletion log",
  file_R_7 = enum_list, sheet_name_7 = "Enumerators list",
  file_R_8 = df_survey, sheet_name_8 = "Kobo survey",
  file_R_9 = df_choices, sheet_name_9 = "Kobo choices",
  export_file_name = paste0("JMMI - ", format(Sys.Date(), "%B-%Y"), "- Logbook for submission"),
  export_directory = "outputs/Logbook for submission/")




cat("\n")
cat("\n")
cat(green$bold("Logbook for submision exported"))

