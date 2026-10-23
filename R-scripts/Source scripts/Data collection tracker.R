cat("\014")  


data_collected <- data


price_collected_tracker_long <- data_collected |> select(organisation, sudan_state, sudan_district, ends_with("_price"), -contains("/")) |> 
  pivot_longer(cols = ends_with("price"), values_drop_na = T, names_to = "Items") |> 
  group_by(organisation, sudan_state, sudan_district, Items) |> 
  summarise(`Number of prices collected` = n()) |> 
  mutate(Items = str_to_title(Items) |> str_replace_all("_", " ") |> 
           str_remove_all(" price")) |> 
  add_administrative_names(sudan_states_and_districts)

price_collected_tracker_wide <- data_collected |> select(organisation, sudan_state, sudan_district, ends_with("_price"), -contains("/")) |> 
  pivot_longer(cols = ends_with("price"), values_drop_na = T, names_to = "Items") |> 
  group_by(organisation, sudan_state, sudan_district, Items) |> 
  summarise(`Number of prices collected` = n()) |> 
  mutate(Items = str_to_title(Items) |> str_replace_all("_", " ") |> 
           str_remove_all(" price")) |> 
  pivot_wider(names_from = Items, values_from = `Number of prices collected`) |> 
  add_administrative_names(sudan_states_and_districts)



prices_collected_tracker <- list(price_collected_tracker_long, price_collected_tracker_wide) |> set_names(c("Long format", "Wide format"))




export_coverage <- function(df_list, export_file_name, export_directory = "") {
  if (!is.list(df_list) || length(df_list) == 0) {
    stop("df_list must be a non-empty list of dataframes")
  }
  
  wb <- createWorkbook()
  
  header_style <- createStyle(
    fontName = "Arial Narrow", fontSize = 11, fontColour = "white", 
    border = "TopBottomLeftRight", borderColour = "black", 
    halign = "center", fgFill = "#E34443", textDecoration = "bold",
    wrapText = T, valign = "center"
  )
  
  table_body_style <- createStyle(
    fontName = "Arial Narrow", fontSize = 7, 
    border = "TopBottomLeftRight", borderColour = "grey26", borderStyle = "thin", 
    wrapText = FALSE
  )
  
  for (df_name in names(df_list)) {
    df <- df_list[[df_name]]
    
    if (!is.data.frame(df)) next  # Skip non-dataframe elements
    
    if (any(sapply(df, inherits, "POSIXct"))) {
      df <- df %>% mutate(across(where(is.POSIXct), as.character))
    }
    
    addWorksheet(wb, sheetName = df_name)
    writeData(wb, sheet = df_name, df, rowNames = FALSE)
    setColWidths(wb, sheet = df_name, cols = 1:ncol(df), widths = rep(15, ncol(df)))
    setRowHeights(wb, sheet = df_name, rows = 1, heights = 30)
    
    addStyle(wb, sheet = df_name, header_style, rows = 1, cols = 1:ncol(df), gridExpand = TRUE)
    addStyle(wb, sheet = df_name, table_body_style, rows = 2:(nrow(df) + 1), cols = 1:ncol(df), gridExpand = TRUE)
  }
  
  saveWorkbook(wb, file.path(export_directory, export_file_name), overwrite = TRUE)
}




export_coverage(prices_collected_tracker, export_file_name = paste0("Tracker_prices_collected_", format(Sys.time(), "%Y-%m-%d_at_%Hh%M"), ".xlsx"), export_directory = path_cleaning_folder_today)

cat("\n")
cat("\n")
cat(green$bold("Prices collected tracker file exported"))





# 
# export_in_reach_format_multiple_sheets(price_collected_tracker_long, 
#                                        sheet_name_1 = "Tracker long", 
#                                        file_R_2 = price_collected_tracker_wide,
#                                        sheet_name_2 = "Tracker wide",
#                                        export_file_name = paste0("Tracker Items Collected up to ", Sys.Date()), 
#                                        export_directory = "outputs/")


