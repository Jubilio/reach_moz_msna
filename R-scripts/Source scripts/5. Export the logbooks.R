cat("\014")  

all_dfs_names_before <- Filter(function(x) is(x, "data.frame"), mget(ls())) |> names()
all_lists_names_before <- Filter(function(x) is(x, "list"), mget(ls())) |> names()




(cl_name_to_export <- paste0(path_cleaning_folder_today, "/Cleaning_log_", format(Sys.time(), "%Y-%m-%d_at_%Hh%M"), ".xlsx")) #|> str_replace_all(" ", "_")

#Export logbook----
cleaningtools::create_xlsx_cleaning_log(
  combined_log, cleaning_log_name = "cleaning_log",
  output_path = cl_name_to_export,
  column_for_color = "issue"
)








cat("\n")
cat("\n")
cat("\n")
cat("\n")
cat("\n")
cat("\n")
cat("\n")
cat(green$bold("Logbook export completed"))
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






