cat("\014")  


all_dfs_names_before <- Filter(function(x) is(x, "data.frame"), mget(ls())) |> names()
all_lists_names_before <- Filter(function(x) is(x, "list"), mget(ls())) |> names()


#Add the flags in Arabic----
#Remove the flags that we detected in previous checks----
old_cls_paths <- list.files("outputs/", pattern = "^Cleaning_log_", full.names = T, recursive = T)
old_cls <- lapply(old_cls_paths, function(x) read_xlsx(x, sheet = "cleaning_log")) %>% bind_rows()

all_old_cls <- old_cls[!duplicated(old_cls[, c("uuid", "question")]),]

data %>% select(where(is.character)) %>% 
  pivot_longer(cols = -c(`_uuid`, sudan_district, sudan_district_en, enumerator_id), 
               values_drop_na = T, values_to = "old_value") -> text_long_df


clog_arabic_tanslation <- text_long_df %>% 
  filter(grepl("[\u0600-\u06FF]", old_value)) %>% mutate(new_value = NA, 
                                                         change_type = "change_response", 
                                                         issue = "Arabic translation needed", 
                                                         check_id = NA, 
                                                         check_binding = NA, 
                                                         name_marketplace = NA) %>% 
  rename("question"="name",
         "uuid"='_uuid') %>% 
  select(all_of(colnames(all_old_cls)))


clog_arabic_tanslation <- clog_arabic_tanslation %>% anti_join(all_old_cls, by = c("uuid","question"))

clog_arabic_tanslation_list <- list("checked_dataset"=data,
                                    "cleaning_log"= clog_arabic_tanslation)



create_xlsx_cleaning_log(clog_arabic_tanslation_list,
                         cleaning_log_name = "cleaning_log",
                         column_for_color = "question",
                         output_path = paste0(path_cleaning_folder_today,"/Cleaning_log_arabic_translation_", format(Sys.time(), "%Y-%m-%d_at_%Hh%M"), ".xlsx"))

cat(green$bold("Cleaning log for arabic translation exported"))


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


