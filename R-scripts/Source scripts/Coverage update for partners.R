cat("\014")  


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


# Sys.setlocale("LC_TIME", "en_EN")

# coverage_update <- data |> 
#   select(organisation, `_submission_time`, sudan_district_en) |> 
#   mutate(date = as.Date(str_remove(`_submission_time`, "T.*")) |> format("%b %d") ) |> 
#   select(-`_submission_time`) |> 
#   group_by_all() |> 
#   summarise(count = n(), .groups = "keep") |> 
#   pivot_wider(names_from = date, values_from = count) |> 
#   ungroup() |> 
#   rename_with(.cols = -c(organisation, sudan_district_en), .fn = ~paste0(.x, "th forms")) |> 
#   rename("Area"="sudan_district_en", "Org"="organisation") |> 
#   group_split(Org)


coverage_update <- data |> 
  select(organisation, `_submission_time`, sudan_district_en) |> 
  mutate(date = as.Date(str_remove(`_submission_time`, "T.*")),
         organisation = str_to_upper(organisation)) |>
  select(-`_submission_time`) |> 
  group_by_all() |> 
  summarise(count = n(), .groups = "keep") |> 
  arrange(organisation, date) |> 
  pivot_wider(names_from = date, values_from = count, names_vary = "slowest") |> 
  ungroup() 
# Reorder columns to keep date columns in chronological order
coverage_update <- coverage_update |> select(organisation, sudan_district_en, 
                                             all_of(sort(names(coverage_update)[!names(coverage_update) %in% c("organisation", "sudan_district_en")]))) |> 
  # Now format the column names
  rename_with(.cols = -c(organisation, sudan_district_en), 
              .fn = ~paste0(format(as.Date(.x), "%b %d"), "th forms")) |> 
  rename("Area" = "sudan_district_en", "Org" = "organisation") |> 
  group_split(Org)



coverage_update <- lapply(coverage_update, \(x) select(x, -where(naniar::all_miss)))
coverage_update <- lapply(coverage_update, \(y) mutate(y, across(where(is.numeric),  ~replace_na(.x, 0))))

coverage_update <- lapply(coverage_update, \(x) x |> rowwise() |> mutate(`Total forms` = sum(c_across(where(is.numeric)))))

names(coverage_update) <- data$organisation |> unique() |> sort()



export_coverage(coverage_update, export_file_name = paste0("Coverage_update_",format(Sys.time(), "%Y-%m-%d_at_%Hh%M"), ".xlsx"), export_directory = path_cleaning_folder_today)

cat("\n")
cat("\n")
cat(green$bold("Coverage update file exported"))


