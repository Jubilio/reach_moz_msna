cat("\014")  

all_dfs_names_before <- Filter(function(x) is(x, "data.frame"), mget(ls())) |> names()
all_lists_names_before <- Filter(function(x) is(x, "list"), mget(ls())) |> names()



#REVIEW AND CLEAN DATASET GENERATION ---------------------------------------------------------------------------------------------

cleaning_log_errors <- cleaningtools::review_cleaning_log(
  raw_dataset = raw_data,
  raw_data_uuid_column = "_uuid",
  cleaning_log = cleaning_log,
  cleaning_log_change_type_column = "change_type",
  change_response_value = "change_response",
  cleaning_log_question_column = "question",
  cleaning_log_uuid_column = "uuid",
  cleaning_log_new_value_column = "new_value"
)



table(data$sudan_district_en)


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
print(cleaning_log_errors)







