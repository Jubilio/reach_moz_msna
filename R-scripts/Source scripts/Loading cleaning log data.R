

#merging the cleaning logs 


# required libraries
library(readxl)
library(dplyr)
library(writexl)

#  folder path
folder <- "inputs/JMMI data cleaning WO feedback"
input_folder_path <- "inputs"

# List all Excel files (.xlsx)
files <- list.files(folder, pattern = "\\.xlsx$", full.names = TRUE)


# Reading the cleaning_log sheet from each, tagging with its source
all_logs <- lapply(files, function(f) {
  df <- read_excel(f, sheet = "cleaning_log")
  df$source_file <- basename(f)
  df
})


#Binding the cleaning logs into one data frame
combined_log <- bind_rows(all_logs)


#output path in the same inputs folder
out_path <- file.path(input_folder_path, "combined_cleaning_log.xlsx")

#Export to Excel
write_xlsx(
  x = list(
    cleaning_log = combined_log
  ),
  path = out_path
)



# loading the cleaning log data 
path_logbook <- "inputs/combined_cleaning_log.xlsx"
excel_sheets(path_logbook)

excel_sheets(path_data)

#loading code
cleaning_log <- read_xlsx(path_logbook, sheet = "cleaning_log") |> 
  filter(!is.na(change_type))


#assigning the raw data 
raw_data <- data

# data1 <- read_xlsx(path_logbook, sheet = "cleaning_log")


cat(green$bold("Cleaning log combined and loaded successfully"))

