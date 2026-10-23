cat("\014")  


all_dfs_names_before <- Filter(function(x) is(x, "data.frame"), mget(ls())) |> names()
all_lists_names_before <- Filter(function(x) is(x, "list"), mget(ls())) |> names()


#Remove the PII----
pii_list <-check_pii(combined_log$checked_dataset, uuid_column = "_uuid") 

pii_cols <- c(coordinates_cols)

combined_log$checked_dataset <- combined_log$checked_dataset %>% select(-pii_cols)
combined_log$cleaning_log <- combined_log$cleaning_log %>% filter(!question %in% pii_cols)


#Replace the converted price by the unconverted ones

uuid <- combined_log$cleaning_log$uuid
questions <- combined_log$cleaning_log$question
old_value <- combined_log$cleaning_log$old_value
price_not_conv_vector <- c()

for (i in 1:length(uuid)){
  
  if (grepl("price_converted$", questions[i])){
    
    price_not_conv <- data %>% filter(`_uuid`==uuid[i]) %>% select(str_remove(questions[i], "_converted")) %>% pull() %>% print()
    price_not_conv_vector <- c(price_not_conv_vector, price_not_conv)
    
  } else{
    
    ov <- old_value[i]
    price_not_conv_vector <- c(price_not_conv_vector ,ov)
    
  }
  
  
}

combined_log$cleaning_log$old_value <- price_not_conv_vector

combined_log$cleaning_log <- combined_log$cleaning_log %>% 
  mutate(question = if_else(grepl("_price_converted", question), str_remove(question, "_converted$"), question))

# combined_log$cleaning_log %>% get_count(uuid, question) #|> view()

#Remove duplicates final
combined_log$cleaning_log <- combined_log$cleaning_log[!duplicated(combined_log$cleaning_log[,c(1,7)]),] 

#Remove the flags that we detected in previous checks----
(old_cls_paths <- list.files("outputs/", pattern = "^Cleaning_log_20", full.names = T, recursive = T ))


if (length(old_cls_paths)>0){
  
  old_cls <- lapply(old_cls_paths, function(x) read_xlsx(x, sheet = "cleaning_log")) %>% bind_rows()
  
  combined_log$cleaning_log <- combined_log$cleaning_log %>% anti_join(old_cls, by = c("uuid", "question")) 
  
  # combined_log$cleaning_log %>% group_by(issue) %>% tally(sort = T)
  # combined_log$cleaning_log %>% filter(grepl("outlier", issue))%>% get_count(question)
  
}







cat("\n")
cat("\n")
cat("\n")
cat("\n")
cat("\n")
cat("\n")
cat("\n")
cat(green$bold("Cleaning logs and deletion logs processed"))
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


