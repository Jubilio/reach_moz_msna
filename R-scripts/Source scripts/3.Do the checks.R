
cat("\014")  


all_dfs_names_before <- Filter(function(x) is(x, "data.frame"), mget(ls())) |> names()
all_lists_names_before <- Filter(function(x) is(x, "list"), mget(ls())) |> names()



#Add columns with the english name of the districts and states----
data <- add_administrative_names(data, sudan_states_and_districts)

#Add columns to the df (percentage missing, duration and other)----
data <- data %>%
  add_percentage_missing() %>% 
  add_duration()


#Count of 999.0 for each variable----
count_999_by_variable(data) 


#Check percentage missing, duration, duplicates, specific values and others----
other_columns_to_check <- df_survey %>%
  filter(type=="text" & grepl("other", name)) %>%
  filter(name %in% names(data)) %>%
  pull(name)

clog <- data %>% 
  check_percentage_missing(uuid_column = "_uuid", column_to_check = "percentage_missing", strongness_factor = 2) %>%
  check_duration(column_to_check = "duration", uuid_column = "_uuid", lower_bound = 10, higher_bound = 1440) %>%
  check_others(uuid_column = "_uuid", columns_to_check = other_columns_to_check) %>% 
  check_duplicate(uuid_column = "_uuid") %>% 
  check_value(uuid_column = "_uuid", element_name = "checked_dataset", values_to_look = c(999," _999", -990, "-999", "999"))


#Outliers----

#Check the distribution of prices levels by state for each item
median_prices <- data %>%
  select(sudan_state, all_of(price_cols_converted), all_of(price_cols_without_conversion)) %>% 
  pivot_longer(cols = -sudan_state, values_drop_na = T, values_transform = as.numeric) %>% 
  group_by(sudan_state, name) %>% 
  summarise(value = median(value))

ggplot(data = median_prices, aes(x = sudan_state, y = value)) + geom_col() + facet_wrap(vars(name), scales = "free")



cols_not_to_check_outliers <- c(other_columns, check_columns, price_cols_with_conversion , coordinates_cols, 
                                "market_days_open" , "_index",  "start", "end", "start_date", "end_date", 
                                "end_time", "start_time", "duration", calc_columns, check_columns,
                                "days_diff", "percentage_missing", "_id")

clog$potential_outliers <- get_outliers(data, cols_not_to_check_outliers) %>% filter(!is.na(old_value))

get_count(clog$potential_outliers, question) %>% print(n=100)



#Value check for price 0 and 999 answers----
flaged_value_price_zero <- data %>% select(`_uuid`, all_of(price_cols)) %>% 
  check_value(uuid_column = "_uuid", values_to_look = c(0, "0"))

clog$flaged_value_price_zero <- flaged_value_price_zero$flaged_value

#Remove the price zero from the outlier detection since they are manually detected in the logical checks
clog$potential_outliers <- anti_join(clog$potential_outliers, clog$flaged_value_price_zero, by = "uuid", "question")


#LOGICAL CHECKS----

#Market days open above 7 days----
clog <- clog %>% 
  check_logical(uuid_column = "_uuid",
                check_to_perform = "market_days_open>7",
                columns_to_clean = "market_days_open",
                description = "The number of days a week the market is reported to be open is above 7",
                check_id = "market_days_open_above_7")


# # LT5: If the answer to market size is "0" and the enumerator wrote prices for the different items----
# 
# clog <- clog %>%
#   check_logical(uuid_column = "_uuid",
#                 check_to_perform = "market_size==0 & rowSums(select(.,all_of(price_cols)), na.rm=TRUE)>0",
#                 description = "Market size is 0 but the enumerator rpeorted prices for some units sold",
#                 check_id = "market_size_0_logical")

#LT3: If any stock is noted 0----

stock_0 <- data %>%
  select( `_uuid` | ends_with("_stock"))%>%
  check_value(uuid_column = "_uuid", values_to_look = 0)

clog$stock_0 <- stock_0$flaged_value

#LT3: If the state reported as "other"----
clog <- clog %>% 
  check_logical(uuid_column = "_uuid",
                check_to_perform = "sudan_district=='other'",
                columns_to_clean = "sudan_district",
                description = "The district is reported as other",
                check_id = "district_other")


#Create a combined log----
combined_log <- clog %>% create_combined_log()

combined_log <- combined_log %>% 
  add_info_to_cleaning_log(dataset_uuid_column = "_uuid", 
                           cleaning_log_uuid_column = "uuid", 
                           information_to_add = c("enumerator_id", "sudan_district", 
                                                  "sudan_district_en", 
                                                  "name_marketplace" ))

combined_log$cleaning_log <- combined_log$cleaning_log %>% 
  select(uuid, sudan_district, sudan_district_en, name_marketplace,
         enumerator_id, everything()) 

#Add more detail in the description of the duration issue----

suppressWarnings(combined_log$cleaning_log$issue <-   ifelse(combined_log$cleaning_log$issue=="Duration is lower or higher than the thresholds" & as.numeric(word(combined_log$cleaning_log$old_value, 1)) > 120, "The survey took more than 120 minutes", combined_log$cleaning_log$issue))
suppressWarnings(combined_log$cleaning_log$issue <-   ifelse(combined_log$cleaning_log$issue=="Duration is lower or higher than the thresholds" & as.numeric(word(combined_log$cleaning_log$old_value, 1)) < 10, "The survey took less than 10 minutes", combined_log$cleaning_log$issue))

#Check outliers with predefined list----
list_of_check <- read_xlsx("inputs/prices_and_stocks_logical_checklist.xlsx")


list_of_check <- list_of_check |> 
  filter(columns_to_clean %in% colnames(data))

cl_outliers_logical <- check_logical_with_list(
  
  data,
  uuid_column = "_uuid",
  information_to_add = NULL,
  list_of_check,
  check_id_column = "check_id",
  check_to_perform_column = "check_to_perform",
  columns_to_clean_column = "columns_to_clean",
  description_column = "descriription",
  bind_checks = TRUE
)


cl_outliers_logical$logical_all %>%
  inner_join(combined_log$cleaning_log, by = c("uuid", "question"))


cl_outliers_logical$logical_all <- cl_outliers_logical$logical_all %>%
  anti_join(combined_log$cleaning_log, by = c("uuid", "question"))

clog$outliers_checklist <- cl_outliers_logical$logical_all



#Create a combined log for the outliers with a checklist----
combined_log_outliers_logical <- cl_outliers_logical %>% create_combined_log()

combined_log_outliers_logical <- combined_log_outliers_logical %>% 
  add_info_to_cleaning_log(dataset_uuid_column = "_uuid", 
                           cleaning_log_uuid_column = "uuid", 
                           information_to_add = c("enumerator_id", "sudan_district", 
                                                  "sudan_district_en", 
                                                  "name_marketplace" ))

combined_log_outliers_logical$cleaning_log <- combined_log_outliers_logical$cleaning_log %>% 
  select(uuid, sudan_district, sudan_district_en, 
         enumerator_id, everything()) 

#Remove the duplicates----
combined_log_outliers_logical$cleaning_log <- combined_log_outliers_logical$cleaning_log %>% 
  anti_join(combined_log$cleaning_log, by = c("uuid", "question"))

#Bind the two comnbined logs----

combined_log$cleaning_log <- rbind(combined_log$cleaning_log, combined_log_outliers_logical$cleaning_log)












cat("\n")
cat("\n")
cat("\n")
cat("\n")
cat("\n")
cat("\n")
cat("\n")
cat(green$bold("Checks completed"))
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














