cat("\014")  

all_dfs_names_before <- Filter(function(x) is(x, "data.frame"), mget(ls())) |> names()
all_lists_names_before <- Filter(function(x) is(x, "list"), mget(ls())) |> names()


#REVIEW AND CLEAN DATASET GENERATION ---------------------------------------------------------------------------------------------

## Household/ main submission
cleaning_log_errors_main <- cleaningtools::review_cleaning_log(
  raw_dataset = data_hh_main,
  raw_data_uuid_column = "_uuid",
  cleaning_log = cleaning_log_main,
  cleaning_log_change_type_column = "change_type",
  change_response_value = "change_response",
  cleaning_log_question_column = "question",
  cleaning_log_uuid_column = "uuid",
  cleaning_log_new_value_column = "new_value"
)


# table(data_hh_main$admin2)



## Education_ind
cleaning_log_errors_education <- cleaningtools::review_cleaning_log(
  raw_dataset = data_edu_ind,
  raw_data_uuid_column = "repeat_index",
  cleaning_log = cleaning_log_edu_ind,
  cleaning_log_change_type_column = "change_type",
  change_response_value = "change_response",
  cleaning_log_question_column = "question",
  cleaning_log_uuid_column = "repeat_index",
  cleaning_log_new_value_column = "new_value"
)


## Health_ind
cleaning_log_errors_health <- cleaningtools::review_cleaning_log(
  raw_dataset = data_health_ind,
  raw_data_uuid_column = "repeat_index",
  cleaning_log = cleaning_log_health_ind,
  cleaning_log_change_type_column = "change_type",
  change_response_value = "change_response",
  cleaning_log_question_column = "question",
  cleaning_log_uuid_column = "repeat_index",
  cleaning_log_new_value_column = "new_value"
)


## Roster_ind
cleaning_log_errors_roster <- cleaningtools::review_cleaning_log(
  raw_dataset = data_roster,
  raw_data_uuid_column = "repeat_index",
  cleaning_log = cleaning_log_roster,
  cleaning_log_change_type_column = "change_type",
  change_response_value = "change_response",
  cleaning_log_question_column = "question",
  cleaning_log_uuid_column = "repeat_index",
  cleaning_log_new_value_column = "new_value"
)


## Nutrition_ind
cleaning_log_errors_nut <- cleaningtools::review_cleaning_log(
  raw_dataset = data_nutrition_ind,
  raw_data_uuid_column = "repeat_index",
  cleaning_log = cleaning_log_nutrition,
  cleaning_log_change_type_column = "change_type",
  change_response_value = "change_response",
  cleaning_log_question_column = "question",
  cleaning_log_uuid_column = "repeat_index",
  cleaning_log_new_value_column = "new_value"
)


## Protection_ind
# cleaning_log_errors <- cleaningtools::review_cleaning_log(
#   raw_dataset = data_child_protection_ind,
#   raw_data_uuid_column = "repeat_index",
#   cleaning_log = cleaning_log,
#   cleaning_log_change_type_column = "change_type",
#   change_response_value = "change_response",
#   cleaning_log_question_column = "question",
#   cleaning_log_uuid_column = "uuid",
#   cleaning_log_new_value_column = "new_value"
# )





cat("\n")
cat("\n")
cat("\n")
cat("\n")
cat(green$bold("Review of the cleaning log completed"))
cat("\n")
cat("\n")
cat("\n")






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
print(cleaning_log_errors_main)
print(cleaning_log_errors_education)
print(cleaning_log_errors_health)
print(cleaning_log_errors_roster)
print(cleaning_log_errors_nut)
# print(cleaning_log_errors)



beepr::beep()






