cat("\014")  


if(!dir_exists("outputs/Logbook for submission")){
  dir_create("outputs/Logbook for submission")
}


#Export the final outputs, raw df, clean df, clean df converted, and all cleaning/deletion logs

export_in_reach_format_multiple_sheets(
  # RAW DATASETS
  file_R_1 = data_hh_main, sheet_name_1 = "raw_data_main",
  file_R_2 = data_hh_roster, sheet_name_2 = "raw_roster",
  file_R_3 = data_edu_ind, sheet_name_3 = "raw_edu_ind", 
  file_R_4 = data_health_ind, sheet_name_4 = "raw_health_ind",
  file_R_5 = data_nutrition_ind, sheet_name_5 = "raw_nutrition_ind",
  file_R_6 = data_child_protection_ind, sheet_name_6 = "raw_child_protection_ind",
  
  # CLEAN DATASETS
  file_R_7 = clean_main, sheet_name_7 = "clean_main",
  file_R_8 = clean_roster, sheet_name_8 = "clean_roster",
  file_R_9 = clean_edu_ind, sheet_name_9 = "clean_edu_ind",
  file_R_10 = clean_health_ind, sheet_name_10 = "clean_health_ind",
  file_R_11 = clean_nutrition_ind, sheet_name_11 = "clean_nutrition_ind",
  file_R_12 = clean_child_protection_ind, sheet_name_12 = "clean_child_protection_ind",
  
  # CLEANING LOGS (WITHOUT DELETIONS)
  file_R_13 = cleaning_log_main_no_dl, sheet_name_13 = "cleaning_log_main",
  file_R_14 = cleaning_log_roster_no_dl, sheet_name_14 = "cleaning_log_roster",
  file_R_15 = cleaning_log_edu_ind_no_dl, sheet_name_15 = "cleaning_log_edu_ind",
  file_R_16 = cleaning_log_health_ind_no_dl, sheet_name_16 = "cleaning_log_health",
  file_R_17 = cleaning_log_nutrition_no_dl, sheet_name_17 = "cleaning_log_nutrition",
  
  # DELETION LOGS
  file_R_18 = deletion_log_main, sheet_name_18 = "deletion_log_main",
  file_R_19 = deletion_log_roster, sheet_name_19 = "deletion_log_roster",
  file_R_20 = deletion_log_educ, sheet_name_20 = "deletion_log_edu_ind",
  file_R_21 = deletion_log_health, sheet_name_21 = "deletion_log_health_ind",
  file_R_22 = deletion_log_nutrition, sheet_name_22 = "deletion_log_nutrition",
  
  # METADATA
  file_R_23 = variable_tracker, sheet_name_23 = "variable_tracker",
  file_R_24 = df_survey, sheet_name_24 = "survey",
  export_file_name = paste0("MSNA - ", format(Sys.Date(), "%B-%Y"), "- Logbook for submission"),
  export_directory = "outputs/Logbook for submission/")




cat("\n")
cat("\n")
cat(green$bold("Logbook for submision exported"))

beepr::beep()