

#Add administrative names in the begining of the data frame
add_administrative_names <- function(df, adm_df){
  if("admin2" %in% names(df)){
    df <- filter(adm_df, Variable == "sudan_district") %>% right_join(df, by = c("Code" = "admin2")) %>% select(-Variable) %>% 
      rename("admin2_en"="Name", "admin2"="Code")  
  }
  if("admin1" %in% names(df)){
    df <- filter(adm_df, Variable == "sudan_states") %>% right_join(df, by = c("Code" = "admin1")) %>% select(-Variable) %>% 
      rename("admin1_en"="Name", "admin1"="Code")  
  }
  df <- select(df, any_of(c("admin1_en", "admin2_en")), everything())
  return(df)
}





add_administrative_names_map_col <- function(df){
  df <- ungroup(df)
  identify_pcode_columns <- function(x) {
    grepl("^SD[0-9]+", x) }
  
  cols_to_transform <- names(df)[apply(df, 2, function(col) any(identify_pcode_columns(col)))]
  print(cols_to_transform)
  
  if(length(cols_to_transform)>0){
    
    df <- df %>% mutate(across(.cols = any_of(cols_to_transform), 
                               .fns = ~ifelse(as.character(.) %in% names(translation_map), translation_map[as.character(.)], . ), 
                               .names =  "{.col}_en" ))
    df <- df %>% select(ends_with("_en"), everything())
  }
  return(df)
}






#Count of 999
count_999_by_variable <- function(df){
  
  df <- df %>% pivot_longer(cols = everything() , cols_vary = "slowest", values_drop_na = T, values_transform = as.character, names_to = "Variable") %>%
    filter(grepl("^999$|_999|^_999$", value)) %>% group_by(Variable) %>% 
    summarise(`Number of 999` = n()) %>% arrange(desc(`Number of 999`))
  
  return(df)
  
}





#Function to remove 999 and get compute outliers. It will only return the outliers, not the checked dataset
get_outliers <- function(df, cols_not_to_check_outliers){
  
  df <- df %>% mutate(across(everything(), ~ifelse(. %in% c(999, -999, "999"), NA, .)))
  clog_outliers <- df %>% check_outliers(uuid_column = "_uuid", sm_separator = "/", 
                                         columns_not_to_check = cols_not_to_check_outliers, 
                                         kobo_survey = df_survey, kobo_choices = df_choices)
  
  return(clog_outliers$potential_outliers)
  
}





#Get count after grouping, just to save time....

get_count <- function(df, ...){
  col <- enquo(col)
  df <- df %>% group_by(...) %>% 
    summarise(count = n()) %>% 
    arrange(desc(count))
  
  return(df)
  
  
}





what_turned_na <- function(df){
  suppressWarnings({
    df %>%  
      pivot_longer(cols = everything(), values_transform = as.character, values_drop_na = T) %>% 
      mutate(value_num = as.numeric(value)) %>% 
      filter(is.na(value_num)) %>% select(-value_num) %>% 
      print()
  })
}







