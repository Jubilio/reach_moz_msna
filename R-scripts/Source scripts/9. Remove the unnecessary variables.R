cat("\014")  

all_dfs_names_before <- Filter(function(x) is(x, "data.frame"), mget(ls())) |> names()
all_lists_names_before <- Filter(function(x) is(x, "list"), mget(ls())) |> names()



#Remove converted columns (Calculated fields from Kobo) and the PII's
raw_data <- raw_data |> select(-ends_with("price_converted"), -contains("gps"), -name_marketplace, -starts_with("check_"))
clean_df <- clean_df |> select(-ends_with("price_converted"), -contains("gps"), -name_marketplace, -starts_with("check_"))
clean_df_converted <- clean_df_converted |> select(-ends_with("price_converted"), -contains("gps"), -name_marketplace, -starts_with("check_"))

final_cleaning_log_no_dl <- cleaning_log |> filter(change_type!="remove_survey")

# variable_tracker <- read_xlsx("../12. Data cleaning January 2025/HQ submission and review files/Data Cleaning_Master Logbook_January_2025_HQ_submission_updated.xlsx", sheet = 2)
enum_list <- clean_df |> select(`_uuid`, enumerator_id) |> distinct()



cat(green$bold("Obsolete variables removed"))
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
cat("\n")


if(length(setdiff(all_lists_names_after, all_lists_names_before))>0){
  
  print("The new lists created are:")
  cat("\n")
  for (df in setdiff(all_lists_names_after, all_lists_names_before)) {
    
    cat(df)
    cat("\n")
  }
}


