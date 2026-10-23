cat("\014")  

all_dfs_names_before <- Filter(function(x) is(x, "data.frame"), mget(ls())) |> names()
all_lists_names_before <- Filter(function(x) is(x, "list"), mget(ls())) |> names()




clean_df <- cleaningtools::create_clean_data(
  raw_dataset = raw_data,
  raw_data_uuid_column = "_uuid",
  cleaning_log = cleaning_log,
  cleaning_log_change_type_column = "change_type",
  change_response_value = "change_response",
  NA_response_value = "blank_response",
  no_change_value = "no_action",
  remove_survey_value = "remove_survey",
  cleaning_log_question_column = "question",
  cleaning_log_uuid_column = "uuid",
  cleaning_log_new_value_column = "new_value"
)





# Recreate parent columns----
clean_df_pc <- cleaningtools::recreate_parent_column(dataset = clean_df, 
                                                     uuid_column = "_uuid", 
                                                     sm_separator = "/", 
                                                     kobo_survey = df_survey, 
                                                     kobo_choices = df_choices,
                                                     cleaning_log_to_append = cleaning_log)


clean_df <- clean_df_pc$data_with_fix_concat
cleaning_log <- clean_df_pc$cleaning_log


#=======my checking code========================
# sapply(clean_df, class)
# 
# any(sapply(clean_df, is.list))
# 
# # See which expected variables in units are missing from clean_df
# setdiff(units$variable, names(clean_df))


# Filter units reference to only include variables that exist in the cleaned data
units_for_conversion <- units[!is.na(units$variable) & units$variable %in% names(clean_df), ]

# Run conversion based on unit references
clean_df_converted <- convert_df(clean_df, units_for_conversion)

#=============================================

# clean_df_converted <- convert_df(clean_df, units)

cleaning_log_no_dl <- cleaning_log |> filter(change_type != "remove_survey")
deletion_log <- cleaning_log |> filter(change_type == "remove_survey") 



# Review cleaning 
suppressWarnings(compared_df <- review_cleaning(
  raw_dataset = raw_data,
  raw_dataset_uuid_column = "_uuid",
  clean_dataset = clean_df_pc$data_with_fix_concat,
  clean_dataset_uuid_column = "_uuid",
  cleaning_log = cleaning_log_no_dl,
  cleaning_log_uuid_column = "uuid",
  cleaning_log_question_column = "question",
  cleaning_log_new_value_column = "new_value",
  cleaning_log_old_value_column = "old_value", 
  cleaning_log_change_type_column = "change_type",
  deletion_log = deletion_log,
  deletion_log_uuid_column = "uuid",
  check_for_deletion_log = T
))

# summary(data$duration)
# 
# library(psych)
# describe(data$duration)



list_compared_df <- compared_df %>% nest_by(comment)

cat("\n")
cat("\n")
cat("\n")
cat("\n")
cat(green$bold("Clean DF and cleaning review generation completed"))
cat("\n")
cat("\n")


path_review_folder <- paste("outputs/JMMI", format(Sys.Date(), "%B"), "cleaning review")

if (!dir_exists(path_review_folder)){
  dir_create(path_review_folder)
}

# Export errors
if (nrow(compared_df)>0){
  
  list_to_export <- group_split(compared_df, comment) |> set_names(substr(sort(unique(compared_df$comment)),1,30))
  export_in_reach_format_list(list_to_export, export_file_name = paste0("Cleaning errors - ", Sys.Date(),".xlsx"), export_directory = path_review_folder)
  
  cat("\n")
  cat("\n")
  cat("\n")
  cat(green$bold("Error log exported"))
  
}












all_dfs_names_after <- Filter(function(x) is(x, "data.frame"), mget(ls())) |> names()
all_lists_names_after <- Filter(function(x) is(x, "list"), mget(ls())) |> names()

if(length(setdiff(all_dfs_names_after, all_dfs_names_before))>0){
  cat(blue("The new dataframes created are:"))
  cat("\n")
  for (df in setdiff(all_dfs_names_after, all_dfs_names_before)) {
    
    cat(df)
    cat("\n")
  }
}


if(length(setdiff(all_lists_names_after, all_lists_names_before))>0){
  
  cat(blue("The new lists created are:"))
  cat("\n")
  for (df in setdiff(all_lists_names_after, all_lists_names_before)) {
    
    cat(df)
    cat("\n")
  }
}





cat("\n")
cat("\n")
cat("\n")
compared_df %>% get_count(comment)

