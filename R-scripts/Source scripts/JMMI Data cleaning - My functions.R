






#Extract base units from the survey sheet in the Kobo tool file

base_units_extraction <- function(survey_sheet){
  
  base_units_extracted <- survey_sheet %>% filter(grepl("Do you sell", `label::English (en)`)) %>% 
    mutate(`base unit` = str_extract(`label::English (en)`, "(?<=\\b(?:in |of |by ))(.*?)(?=\\?)")) %>% 
    select(name,`base unit`)
  
  return(base_units_extracted)
}





#Extract every non standard units of measurement from the data frame

all_units_of_measurements <- function(df){
  
  all_units <- data.frame("item" = character(0), "unit" = character(0))
  
  unit_conv_cols <- df %>% select(contains("_conv_")|contains("_unit_")) %>% colnames()
  units_appended <- c()
  for (i in unit_conv_cols){
    current_unit_col <- df[i]
    names(current_unit_col) <- "unit"
    current_item <- i
    #current_item <- str_remove(i, "_unit_other$")
    #print(current_item)
    current_item_col <- as_tibble(data.frame(item= rep(current_item, nrow(current_unit_col))))
    df_to_append <- cbind(current_item_col, current_unit_col)
    
    all_units <- rbind(all_units, df_to_append)
    units_appended <- append(units_appended, current_item)
    
  }
  
  all_units <- all_units %>% filter(!is.na(unit)) %>% 
    unique(.) %>% 
    #filter(!grepl("firewood", item)) %>% 
    filter(!grepl("other",unit)) %>% 
    filter(!grepl("999",unit))
  
  return(all_units)
  
}



#Add administrative names in the begining of the data frame
add_administrative_names <- function(df, adm_df){
  
  df <- df %>%  
    left_join(filter(adm_df, Variable=="sudan_district"), by = c("sudan_district"="Code")) %>% 
    select(-Variable) %>% 
    rename("sudan_district_en"="Name") %>% 
    left_join(filter(adm_df, Variable=="sudan_states"), by = c("sudan_state"="Code")) %>% 
    select(-Variable) %>% 
    rename("sudan_state_en"="Name") %>% 
    select(sudan_state_en, sudan_district_en, everything())
  
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




#NEW CONVERSION FUNCTIONS----

convert_price <- function(price, unit_col_name, unit_value, conversion_df){
  
  if (is.na(price)) {
    return(NA)
  }
  conversion_factor <- sapply(seq_along(unit_value), function(i){
    
    if (is.na(unit_value)){
      return(1)
    }
    
    else {
      #Find the conversion factor for the given unit
      match <- conversion_df[tolower(trimws(conversion_df$variable)) == tolower(trimws(tolower(unit_col_name))) &
                               tolower(trimws(conversion_df$value)) == tolower(trimws(unit_value[i])), "conversion"]
      
      match <- as.numeric(match)
      
      if (length(match)== 1){
        
        return(match)
      } else{
        
        if (length(match > 1)){
          print("There are more than one matches, please check your converison file")
          
        } else {return(1)} 
      } 
    }
  })
  #Multiply the price by the converted price
  converted_price <- price * conversion_factor
  return(converted_price)
}


convert_df <- function(df, conversion_df){
  df <- add_base_units(df)
  
  #select unit columns
  unit_pattern <- "_unit_list|_unit_other"
  unit_columns <- colnames(select(df, matches(unit_pattern)))
  
  for (unit_col in unit_columns){
    #Get price column
    item_name <- str_remove(unit_col, unit_pattern)
    price_column <- paste(item_name, "_price", sep = "")
    
    #Check if the price columns exist in the data
    if (price_column %in% names(df)){
      print(price_column)
      print(unit_col)
      df[[price_column]] <- sapply(seq_along(df[[price_column]]),
                                   function(i){
                                     convert_price(df[[price_column]][i], unit_col, df[[unit_col]][i], conversion_df)
                                   })
      
    }
    else{print(paste(price_column, "not in dataset.", sep = " "))}
  }
  
  df <- df %>% mutate(across(matches("conv_"), as.numeric)) %>% 
    mutate(
      gas_cylinder_price = if_else(!is.na(gas_cylinder_conv_kg), 12.5 * gas_cylinder_price / gas_cylinder_conv_kg, gas_cylinder_price),
      gas_refill_price = if_else(!is.na(gas_refill_conv_kg), 12.5 * gas_refill_price / gas_refill_conv_kg, gas_refill_price),
      water_container_price = if_else(!is.na(water_container_conv_l), 20* water_container_price / water_container_conv_l, water_container_price),
      water_supply_price = if_else(!is.na(water_supply_conv_l), water_supply_price / water_supply_conv_l, water_supply_price),
      body_soap_price = if_else(!is.na(body_soap_conv_gr), 125 * body_soap_price / body_soap_conv_gr, body_soap_price),
      tooth_paste_price = if_else(!is.na(tooth_paste_conv_g), 100 * tooth_paste_price / tooth_paste_conv_g, tooth_paste_price),
      laundry_soap_price = if_else(!is.na(laundry_soap_conv_gr), 200 * laundry_soap_price / laundry_soap_conv_gr, laundry_soap_price),
      liquid_dish_soap_price = if_else(!is.na(liquid_dish_soap_conv_ml), 600* liquid_dish_soap_price / liquid_dish_soap_conv_ml, liquid_dish_soap_price),
      sanitary_pads_price = if_else(!is.na(sanitary_pads_conv_pcs), 8 * sanitary_pads_price / sanitary_pads_conv_pcs, sanitary_pads_price),
      # charcoal_price = if_else(!is.na(charcoal_conv_kg), charcoal_price / charcoal_conv_kg, charcoal_price),
      #table_spoon_price = if_else(table_spoon_conv_12 == "yes", table_spoon_price / 12, table_spoon_price)
    )
  
  
  
  return(df)
}


#Add the base units for the conversion----

add_base_units <- function(df){
  #We add the base non standard unit so it is taken into account in the conversion
  df <- df %>%
    mutate(sorghum_unit_list = ifelse(sorghum_unit == 'yes', 'malwa', sorghum_unit_list),
           onions_unit_list = ifelse(onions_unit == 'yes', 'malwa', onions_unit_list),
           tomato_unit_list = ifelse(tomato_unit == 'yes', '400 g', tomato_unit_list),
           # firewood_unit_list = ifelse(firewood_unit == 'yes', 'hezma (10 pieces)', firewood_unit_list),
           # charcoal_unit_list = ifelse(charcoal_unit == 'yes', 'koum', charcoal_unit_list),
           water_supply_unit_list = ifelse(water_supply_unit == 'yes', '2 jerrycans', water_supply_unit_list),
           milk_unit_other = ifelse(milk_unit == 'yes', 'ratl', milk_unit_other),
           water_container_unit_list = ifelse(water_container_unit == 'yes', '20l', water_container_unit_list))
  
  return(df)
}







export_in_reach_format_list <- function(df_list, export_file_name, export_directory = "") {
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
    setColWidths(wb, sheet = df_name, cols = 1:ncol(df), widths = rep(25, ncol(df)))
    setRowHeights(wb, sheet = df_name, rows = 1, heights = 30)
    
    addStyle(wb, sheet = df_name, header_style, rows = 1, cols = 1:ncol(df), gridExpand = TRUE)
    addStyle(wb, sheet = df_name, table_body_style, rows = 2:(nrow(df) + 1), cols = 1:ncol(df), gridExpand = TRUE)
  }
  
  saveWorkbook(wb, file.path(export_directory, export_file_name), overwrite = TRUE)
}






export_in_reach_format_multiple_sheets<-function(file_R_1,  sheet_name_1 = "sheet_1",
                                                 file_R_2 = NULL,sheet_name_2= "2",
                                                 file_R_3= NULL,  sheet_name_3= "3",
                                                 file_R_4= NULL,sheet_name_4= "4",
                                                 file_R_5= NULL,  sheet_name_5= "5",
                                                 file_R_6= NULL,sheet_name_6= "6",
                                                 file_R_7= NULL,sheet_name_7= "7",
                                                 file_R_8= NULL,sheet_name_8= "8",
                                                 file_R_9= NULL,sheet_name_9= "9",
                                                 file_R_10= NULL,sheet_name_10= "10",
                                                 file_R_11= NULL,sheet_name_11= "11",
                                                 file_R_12= NULL,sheet_name_12= "12",
                                                 file_R_13= NULL,sheet_name_13= "13",
                                                 file_R_14= NULL,sheet_name_14= "14",
                                                 file_R_15= NULL,sheet_name_15= "15",
                                                 file_R_16= NULL,sheet_name_16= "16",
                                                 file_R_17= NULL,sheet_name_17= "17",
                                                 file_R_18= NULL,sheet_name_18= "18",
                                                 file_R_19= NULL,sheet_name_19= "19",
                                                 file_R_20= NULL,sheet_name_20= "20",
                                                 file_R_21= NULL,sheet_name_21= "21",
                                                 file_R_22= NULL,sheet_name_22= "22",
                                                 file_R_23= NULL,sheet_name_23= "23",
                                                 file_R_24= NULL,sheet_name_24= "23",
                                                 export_file_name,
                                                 export_directory = ""){
  
  #set dates back to characther so Excel shows them correctly
  if(length(names(Filter(is.POSIXct,file_R_1 )))>0){
    file_R_1<-file_R_1 %>% mutate_if(is.POSIXct,as.character)
  }
  
  # Create workbook
  wb <- createWorkbook()
  #for file_R_1
  # Add a worksheets
  addWorksheet(wb, sheet = sheet_name_1, gridLines = T)
  # write data
  writeData(wb, sheet = sheet_name_1, file_R_1, rowNames = F)
  # set column widths
  setColWidths(wb, sheet = sheet_name_1, cols = 1:ncol(file_R_1), widths = rep(15,ncol(file_R_1)))
  setRowHeights(wb, sheet = sheet_name_1,rows = 1, heights = 17)
  
  # header style
  header_style <- 
    createStyle(fontName = "Arial Narrow", fontSize = 11, fontColour = "white",  border = "TopBottomLeftRight ", borderColour = "black", halign = "center", fgFill = "#E34443", textDecoration = "bold")
  addStyle(wb, sheet = sheet_name_1, header_style, rows = 1, cols = 1:ncol(file_R_1), gridExpand = TRUE)
  
  # table body style
  table_body_style1 <-
    createStyle(fontName = "Arial Narrow",fontSize = 7, border = "TopBottomLeftRight ", borderColour = "grey26", borderStyle = "thin", wrapText = F)
  addStyle(wb, sheet = sheet_name_1, table_body_style1, rows = 2:(nrow(file_R_1)+1), cols = 1:ncol(file_R_1), gridExpand = TRUE)
  
  
  if(!is.null(file_R_2)){
    
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_2 )))>0){
      file_R_2<-file_R_2 %>% mutate_if(is.POSIXct,as.character)
    }
    
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_2, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_2, file_R_2, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_2, cols = 1:ncol(file_R_2), widths = rep(15,ncol(file_R_2)))
    setRowHeights(wb, sheet = sheet_name_2,rows = 1, heights = 17)
    
    # header style
    addStyle(wb, sheet = sheet_name_2, header_style, rows = 1, cols = 1:ncol(file_R_2), gridExpand = TRUE)
    
    # table body style
    addStyle(wb, sheet = sheet_name_2, table_body_style1, rows = 2:(nrow(file_R_2)+1), cols = 1:ncol(file_R_2), gridExpand = TRUE)
  }
  
  if(!is.null(file_R_3)){
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_3 )))>0){
      file_R_3<-file_R_3 %>% mutate_if(is.POSIXct,as.character)
    }
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_3, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_3, file_R_3, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_3, cols = 1:ncol(file_R_3), widths = rep(15,ncol(file_R_3)))
    setRowHeights(wb, sheet = sheet_name_3,rows = 1, heights = 17)
    
    # header style
    addStyle(wb, sheet = sheet_name_3, header_style, rows = 1, cols = 1:ncol(file_R_3), gridExpand = TRUE)
    
    # table body style
    addStyle(wb, sheet = sheet_name_3, table_body_style1, rows = 2:(nrow(file_R_3)+1), cols = 1:ncol(file_R_3), gridExpand = TRUE)
  }
  
  if(!is.null(file_R_4)){
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_4 )))>0){
      file_R_4<-file_R_4 %>% mutate_if(is.POSIXct,as.character)
    }
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_4, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_4, file_R_4, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_4, cols = 1:ncol(file_R_4), widths = rep(15,ncol(file_R_4)))
    setRowHeights(wb, sheet = sheet_name_4,rows = 1, heights = 17)
    
    # header style
    addStyle(wb, sheet = sheet_name_4, header_style, rows = 1, cols = 1:ncol(file_R_4), gridExpand = TRUE)
    
    # table body style
    addStyle(wb, sheet = sheet_name_4, table_body_style1, rows = 2:(nrow(file_R_4)+1), cols = 1:ncol(file_R_4), gridExpand = TRUE)
  }
  
  if(!is.null(file_R_5)){
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_5 )))>0){
      file_R_5<-file_R_5 %>% mutate_if(is.POSIXct,as.character)
    }
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_5, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_5, file_R_5, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_5, cols = 1:ncol(file_R_5), widths = rep(15,ncol(file_R_5)))
    setRowHeights(wb, sheet = sheet_name_5,rows = 1, heights = 17)
    
    # header style
    addStyle(wb, sheet = sheet_name_5, header_style, rows = 1, cols = 1:ncol(file_R_5), gridExpand = TRUE)
    
    # table body style
    addStyle(wb, sheet = sheet_name_5, table_body_style1, rows = 2:(nrow(file_R_5)+1), cols = 1:ncol(file_R_5), gridExpand = TRUE)
  }
  
  if(!is.null(file_R_6)){
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_6 )))>0){
      file_R_6<-file_R_6 %>% mutate_if(is.POSIXct,as.character)
    }
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_6, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_6, file_R_6, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_6, cols = 1:ncol(file_R_6), widths = rep(15,ncol(file_R_6)))
    setRowHeights(wb, sheet = sheet_name_6,rows = 1, heights = 17)
    
    # header style
    addStyle(wb, sheet = sheet_name_6, header_style, rows = 1, cols = 1:ncol(file_R_6), gridExpand = TRUE)
    
    # table body style
    addStyle(wb, sheet = sheet_name_6, table_body_style1, rows = 2:(nrow(file_R_6)+1), cols = 1:ncol(file_R_6), gridExpand = TRUE)
  }
  
  if(!is.null(file_R_7)){
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_7 )))>0){
      file_R_7<-file_R_7 %>% mutate_if(is.POSIXct,as.character)
    }
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_7, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_7, file_R_7, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_7, cols = 1:ncol(file_R_7), widths = rep(15,ncol(file_R_7)))
    setRowHeights(wb, sheet = sheet_name_7,rows = 1, heights = 17)
    
    # header style
    addStyle(wb, sheet = sheet_name_7, header_style, rows = 1, cols = 1:ncol(file_R_7), gridExpand = TRUE)
    
    # table body style
    addStyle(wb, sheet = sheet_name_7, table_body_style1, rows = 2:(nrow(file_R_7)+1), cols = 1:ncol(file_R_7), gridExpand = TRUE)
  }
  
  if(!is.null(file_R_8)){
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_8 )))>0){
      file_R_8<-file_R_8 %>% mutate_if(is.POSIXct,as.character)
    }
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_8, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_8, file_R_8, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_8, cols = 1:ncol(file_R_8), widths = rep(15,ncol(file_R_8)))
    setRowHeights(wb, sheet = sheet_name_8,rows = 1, heights = 17)
    
    # header style
    addStyle(wb, sheet = sheet_name_8, header_style, rows = 1, cols = 1:ncol(file_R_8), gridExpand = TRUE)
    
    # table body style
    addStyle(wb, sheet = sheet_name_8, table_body_style1, rows = 2:(nrow(file_R_8)+1), cols = 1:ncol(file_R_8), gridExpand = TRUE)
  }
  
  if(!is.null(file_R_9)){
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_9 )))>0){
      file_R_9<-file_R_9 %>% mutate_if(is.POSIXct,as.character)
    }
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_9, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_9, file_R_9, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_9, cols = 1:ncol(file_R_9), widths = rep(15,ncol(file_R_9)))
    setRowHeights(wb, sheet = sheet_name_9,rows = 1, heights = 17)
    
    # header style
    addStyle(wb, sheet = sheet_name_9, header_style, rows = 1, cols = 1:ncol(file_R_9), gridExpand = TRUE)
    
    # table body style
    addStyle(wb, sheet = sheet_name_9, table_body_style1, rows = 2:(nrow(file_R_9)+1), cols = 1:ncol(file_R_9), gridExpand = TRUE)
  }
  
  if(!is.null(file_R_10)){
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_10 )))>0){
      file_R_10<-file_R_10 %>% mutate_if(is.POSIXct,as.character)
    }
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_10, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_10, file_R_10, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_10, cols = 1:ncol(file_R_10), widths = rep(15,ncol(file_R_10)))
    setRowHeights(wb, sheet = sheet_name_10,rows = 1, heights = 17)
    
    # header style
    addStyle(wb, sheet = sheet_name_10, header_style, rows = 1, cols = 1:ncol(file_R_10), gridExpand = TRUE)
    
    # table body style
    addStyle(wb, sheet = sheet_name_10, table_body_style1, rows = 2:(nrow(file_R_10)+1), cols = 1:ncol(file_R_10), gridExpand = TRUE)
  }
  
  
  if(!is.null(file_R_11)){
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_11 )))>0){
      file_R_11<-file_R_11 %>% mutate_if(is.POSIXct,as.character)
    }
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_11, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_11, file_R_11, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_11, cols = 1:ncol(file_R_11), widths = rep(15,ncol(file_R_11)))
    setRowHeights(wb, sheet = sheet_name_11,rows = 1, heights = 17)
    
    # header style
    addStyle(wb, sheet = sheet_name_11, header_style, rows = 1, cols = 1:ncol(file_R_11), gridExpand = TRUE)
    
    # table body style
    addStyle(wb, sheet = sheet_name_11, table_body_style1, rows = 2:(nrow(file_R_11)+1), cols = 1:ncol(file_R_11), gridExpand = TRUE)
  }
  
  
  if(!is.null(file_R_12)){
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_12 )))>0){
      file_R_12<-file_R_12 %>% mutate_if(is.POSIXct,as.character)
    }
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_12, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_12, file_R_12, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_12, cols = 1:ncol(file_R_12), widths = rep(15,ncol(file_R_12)))
    setRowHeights(wb, sheet = sheet_name_12,rows = 1, heights = 17)
    
    # header style
    addStyle(wb, sheet = sheet_name_12, header_style, rows = 1, cols = 1:ncol(file_R_12), gridExpand = TRUE)
    
    # table body style
    addStyle(wb, sheet = sheet_name_12, table_body_style1, rows = 2:(nrow(file_R_12)+1), cols = 1:ncol(file_R_12), gridExpand = TRUE)
  }
  
  
  if(!is.null(file_R_13)){
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_13 )))>0){
      file_R_13<-file_R_13 %>% mutate_if(is.POSIXct,as.character)
    }
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_13, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_13, file_R_13, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_13, cols = 1:ncol(file_R_13), widths = rep(15,ncol(file_R_13)))
    setRowHeights(wb, sheet = sheet_name_13,rows = 1, heights = 17)
    
    # header style
    addStyle(wb, sheet = sheet_name_13, header_style, rows = 1, cols = 1:ncol(file_R_13), gridExpand = TRUE)
    
    # table body style
    addStyle(wb, sheet = sheet_name_13, table_body_style1, rows = 2:(nrow(file_R_13)+1), cols = 1:ncol(file_R_13), gridExpand = TRUE)
  }
  
  
  if(!is.null(file_R_14)){
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_14 )))>0){
      file_R_14<-file_R_14 %>% mutate_if(is.POSIXct,as.character)
    }
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_14, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_14, file_R_14, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_14, cols = 1:ncol(file_R_14), widths = rep(15,ncol(file_R_14)))
    setRowHeights(wb, sheet = sheet_name_14,rows = 1, heights = 17)
    
    # header style
    addStyle(wb, sheet = sheet_name_14, header_style, rows = 1, cols = 1:ncol(file_R_14), gridExpand = TRUE)
    
    # table body style
    addStyle(wb, sheet = sheet_name_14, table_body_style1, rows = 2:(nrow(file_R_14)+1), cols = 1:ncol(file_R_14), gridExpand = TRUE)
  }
  
  
  if(!is.null(file_R_15)){
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_15 )))>0){
      file_R_15<-file_R_15 %>% mutate_if(is.POSIXct,as.character)
    }
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_15, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_15, file_R_15, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_15, cols = 1:ncol(file_R_15), widths = rep(15,ncol(file_R_15)))
    setRowHeights(wb, sheet = sheet_name_15,rows = 1, heights = 17)
    
    # header style
    addStyle(wb, sheet = sheet_name_15, header_style, rows = 1, cols = 1:ncol(file_R_15), gridExpand = TRUE)
    
    # table body style
    addStyle(wb, sheet = sheet_name_15, table_body_style1, rows = 2:(nrow(file_R_15)+1), cols = 1:ncol(file_R_15), gridExpand = TRUE)
  }
  
  
  if(!is.null(file_R_16)){
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_16 )))>0){
      file_R_16<-file_R_16 %>% mutate_if(is.POSIXct,as.character)
    }
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_16, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_16, file_R_16, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_16, cols = 1:ncol(file_R_16), widths = rep(15,ncol(file_R_16)))
    setRowHeights(wb, sheet = sheet_name_16,rows = 1, heights = 17)
    
    # header style
    addStyle(wb, sheet = sheet_name_16, header_style, rows = 1, cols = 1:ncol(file_R_16), gridExpand = TRUE)
    
    # table body style
    addStyle(wb, sheet = sheet_name_16, table_body_style1, rows = 2:(nrow(file_R_16)+1), cols = 1:ncol(file_R_16), gridExpand = TRUE)
  }
  
  
  if(!is.null(file_R_17)){
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_17 )))>0){
      file_R_17<-file_R_17 %>% mutate_if(is.POSIXct,as.character)
    }
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_17, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_17, file_R_17, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_17, cols = 1:ncol(file_R_17), widths = rep(15,ncol(file_R_17)))
    setRowHeights(wb, sheet = sheet_name_17,rows = 1, heights = 17)
    
    # header style
    addStyle(wb, sheet = sheet_name_17, header_style, rows = 1, cols = 1:ncol(file_R_17), gridExpand = TRUE)
    
    # table body style
    addStyle(wb, sheet = sheet_name_17, table_body_style1, rows = 2:(nrow(file_R_17)+1), cols = 1:ncol(file_R_17), gridExpand = TRUE)
  }
  
  
  if(!is.null(file_R_18)){
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_18 )))>0){
      file_R_18<-file_R_18 %>% mutate_if(is.POSIXct,as.character)
    }
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_18, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_18, file_R_18, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_18, cols = 1:ncol(file_R_18), widths = rep(15,ncol(file_R_18)))
    setRowHeights(wb, sheet = sheet_name_18,rows = 1, heights = 17)
    
    # header style
    addStyle(wb, sheet = sheet_name_18, header_style, rows = 1, cols = 1:ncol(file_R_18), gridExpand = TRUE)
    
    # table body style
    addStyle(wb, sheet = sheet_name_18, table_body_style1, rows = 2:(nrow(file_R_18)+1), cols = 1:ncol(file_R_18), gridExpand = TRUE)
  }
  
  
  if(!is.null(file_R_19)){
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_19 )))>0){
      file_R_19<-file_R_19 %>% mutate_if(is.POSIXct,as.character)
    }
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_19, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_19, file_R_19, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_19, cols = 1:ncol(file_R_19), widths = rep(15,ncol(file_R_19)))
    setRowHeights(wb, sheet = sheet_name_19,rows = 1, heights = 17)
    
    # header style
    addStyle(wb, sheet = sheet_name_19, header_style, rows = 1, cols = 1:ncol(file_R_19), gridExpand = TRUE)
    
    # table body style
    addStyle(wb, sheet = sheet_name_19, table_body_style1, rows = 2:(nrow(file_R_19)+1), cols = 1:ncol(file_R_19), gridExpand = TRUE)
  }
  
  
  if(!is.null(file_R_20)){
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_20 )))>0){
      file_R_20<-file_R_20 %>% mutate_if(is.POSIXct,as.character)
    }
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_20, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_20, file_R_20, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_20, cols = 1:ncol(file_R_20), widths = rep(15,ncol(file_R_20)))
    setRowHeights(wb, sheet = sheet_name_20,rows = 1, heights = 17)
    
    # header style
    addStyle(wb, sheet = sheet_name_20, header_style, rows = 1, cols = 1:ncol(file_R_20), gridExpand = TRUE)
    
    # table body style
    addStyle(wb, sheet = sheet_name_20, table_body_style1, rows = 2:(nrow(file_R_20)+1), cols = 1:ncol(file_R_20), gridExpand = TRUE)
  }
  
  
  if(!is.null(file_R_21)){
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_21 )))>0){
      file_R_21<-file_R_21 %>% mutate_if(is.POSIXct,as.character)
    }
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_21, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_21, file_R_21, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_21, cols = 1:ncol(file_R_21), widths = rep(15,ncol(file_R_21)))
    setRowHeights(wb, sheet = sheet_name_21,rows = 1, heights = 17)
    
    # header style
    addStyle(wb, sheet = sheet_name_21, header_style, rows = 1, cols = 1:ncol(file_R_21), gridExpand = TRUE)
    
    # table body style
    addStyle(wb, sheet = sheet_name_21, table_body_style1, rows = 2:(nrow(file_R_21)+1), cols = 1:ncol(file_R_21), gridExpand = TRUE)
  }
  
  
  if(!is.null(file_R_22)){
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_22 )))>0){
      file_R_22<-file_R_22 %>% mutate_if(is.POSIXct,as.character)
    }
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_22, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_22, file_R_22, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_22, cols = 1:ncol(file_R_22), widths = rep(15,ncol(file_R_22)))
    setRowHeights(wb, sheet = sheet_name_22,rows = 1, heights = 17)
    
    # header style
    addStyle(wb, sheet = sheet_name_22, header_style, rows = 1, cols = 1:ncol(file_R_22), gridExpand = TRUE)
    
    # table body style
    addStyle(wb, sheet = sheet_name_22, table_body_style1, rows = 2:(nrow(file_R_22)+1), cols = 1:ncol(file_R_22), gridExpand = TRUE)
  }
  
  
  if(!is.null(file_R_23)){
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_23 )))>0){
      file_R_23<-file_R_23 %>% mutate_if(is.POSIXct,as.character)
    }
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_23, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_23, file_R_23, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_23, cols = 1:ncol(file_R_23), widths = rep(15,ncol(file_R_23)))
    setRowHeights(wb, sheet = sheet_name_23,rows = 1, heights = 17)
    
    # header style
    addStyle(wb, sheet = sheet_name_23, header_style, rows = 1, cols = 1:ncol(file_R_23), gridExpand = TRUE)
    
    # table body style
    addStyle(wb, sheet = sheet_name_23, table_body_style1, rows = 2:(nrow(file_R_23)+1), cols = 1:ncol(file_R_23), gridExpand = TRUE)
  }
  
  if(!is.null(file_R_24)){
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_24 )))>0){
      file_R_24 <- file_R_24 %>% mutate_if(is.POSIXct,as.character)
    }
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_24, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_24, file_R_24, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_24, cols = 1:ncol(file_R_24), widths = rep(15,ncol(file_R_24)))
    setRowHeights(wb, sheet = sheet_name_24,rows = 1, heights = 17)
    
    # header style
    addStyle(wb, sheet = sheet_name_24, header_style, rows = 1, cols = 1:ncol(file_R_24), gridExpand = TRUE)
    
    # table body style
    addStyle(wb, sheet = sheet_name_24, table_body_style1, rows = 2:(nrow(file_R_24)+1), cols = 1:ncol(file_R_24), gridExpand = TRUE)
  }
  
  
  saveWorkbook(wb, paste0(export_directory,export_file_name,".xlsx"), overwrite = TRUE)
}


























