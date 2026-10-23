all_dfs_names_before <- Filter(function(x) is(x, "data.frame"), mget(ls())) |> names()
all_lists_names_before <- Filter(function(x) is(x, "list"), mget(ls())) |> names()


# Load the data ----

data <- read_xlsx(path_data,
                  # sheet = "checked_dataset", 
                  guess_max = 10000000) 
df_survey <- readxl::read_excel(path_tool, sheet = "survey")
df_choices <- readxl::read_excel(path_tool, sheet = "choices")

sudan_states_and_districts <- read.xlsx("inputs/States_and_Districts.xlsx")
units <- read.xlsx("inputs/Units of measurement.xlsx", sheet = "formatted_corrected")


# Create the variables ----

# Create a data frame to see the base unit of each item
df_base_units <- base_units_extraction(df_survey)

#Create vectors of items, price column names and stock or restock column names
price_cols <- data %>% select(ends_with("price"), -contains("estimate"), -contains("/")) %>% colnames()
price_cols_converted <- data %>% select(ends_with("price_converted")) %>% colnames()
price_cols_with_conversion <- data %>% select(ends_with("_converted")) %>% colnames() %>% str_remove("_converted")
price_cols_without_conversion <- setdiff(price_cols, grep("_converted", colnames(data),value = T) %>% str_remove("_converted"))
items <- price_cols %>% str_remove_all("_price")
stock_restock_cols <- names(data %>% select(ends_with("_stock"), ends_with("_restock")))
food_price_cols <- price_cols[1:9]
coordinates_cols <- data %>% select(contains("gps_")) %>% colnames()
check_columns <- data %>% select(contains("check")) %>% colnames()
other_columns <- data %>% select(ends_with("_other")) %>% colnames()
calc_columns <- data %>% select(contains("calc")) %>% colnames()
integer_cols <- df_survey |> filter(type=="integer") |> pull(name) #from the tool
decimal_cols <- df_survey |> filter(type=="decimal") |> pull(name) #from the tool
binary_cols <- data |> select(contains("/")) |> colnames() #from the tool


suppressWarnings(data <- data |> 
  mutate(across(all_of(ends_with("_price_converted")), as.numeric),
         across(all_of(decimal_cols), as.numeric),
         across(all_of(integer_cols), as.integer),
         across(ends_with("other") & -matches("/"), as.character),
         across(all_of(binary_cols), as.integer),
         across(all_of(stock_restock_cols), as.integer))
)

#Round the price columns----
data <- data |>
  mutate(across(all_of(c(price_cols, price_cols_converted)), round)) 

#Extract all the non base units of measurement from the dataframe----
all_units <- all_units_of_measurements(data)



cat("\n")
cat("\n")
cat("\n")
cat("\n")
cat("\n")
cat("\n")
cat("\n")
cat(green$bold("Data loaded and processed"))
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







