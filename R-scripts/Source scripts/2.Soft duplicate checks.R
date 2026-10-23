cat("\014")  


all_dfs_names_before <- Filter(function(x) is(x, "data.frame"), mget(ls())) |> names()
all_lists_names_before <- Filter(function(x) is(x, "list"), mget(ls())) |> names()


#Check the soft duplicates before any modification of the dataset----
clog_sf <- data %>% 
  check_soft_duplicates(df_survey, 
                        uuid_column = "_uuid", 
                        sm_separator = "/", 
                        log_name = "soft_duplicate", 
                        idnk_value = "999", threshold = 3)

enum_df <- data %>% select(enumerator_id, `_uuid`)

clog_sf$soft_duplicate <- clog_sf$soft_duplicate %>% left_join(enum_df, by = c("uuid" = "_uuid")) %>% 
  select(enumerator_id, everything()) %>% rename("enum_1" = "enumerator_id") %>% 
  left_join(enum_df, by = c("id_most_similar_survey"= "_uuid")) %>% rename("enum_2" = "enumerator_id") 

#Look at the answers flagged in the sf check
# data %>% 
#   filter(`_uuid`=="f2851c4d-6f2f-459d-bb0e-79c6ff0b74c0"|`_uuid`=="1900c316-39d5-45dd-bce6-1deb4d6756af") %>% 
#   t() %>% as.data.frame() %>% filter(!is.na(V1))













cat("\n")
cat("\n")
cat("\n")
cat("\n")
cat("\n")
cat("\n")
cat("\n")
cat(green$bold("Soft duplicates checks completed"))
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













