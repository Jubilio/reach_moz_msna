rm(list = ls())

Sys.setlocale("LC_TIME", "English")
options(scipen=999) 

# Load necessary libraries and functions
library(tidyverse)
library(openxlsx)
library(writexl)
library(readxl)
library(naniar)
library(purrr)
#library(supporteR)
library(cleaningtools)
library(lubridate)
library(fs)
library(crayon)
source("R-scripts/Source scripts/JMMI Data cleaning - My functions.R")


# Create the folders for the analysis in the begining of the data collection
folders <- c("inputs", "outputs", "R-scripts", "archive")

for (folder in folders){
  if (!dir_exists(folder)){
    dir_create(folder)}}

# Create a foder each day of the cleaning ---
path_cleaning_folder_today <- paste0("outputs/JMMI data cleaning ", format(Sys.Date(), format = "%Y-%b-%d"))

if (!dir_exists(path_cleaning_folder_today)){
  dir_create(path_cleaning_folder_today)}



# Define the path of the dataset and tool
path_tool <- "inputs/AUGUST_FINAL_SUDAN_JMMI_2025.xlsx"
(path_data <- "inputs/AUGUST_FINAL_SUDAN_JMMI_2025_-_all_versions_-_False_-_2025-08-27-13-10-46.xlsx")
excel_sheets(path_data)


# Load and process the datasets
source("R-scripts/Source scripts//1.Load and process the data.R")

# Check the soft duplicates
source("R-scripts/Source scripts//2.Soft duplicate checks.R")

# Do the checks
source("R-scripts/Source scripts//3.Do the checks.R")

# Remove the PIIs and process the cleaning outputs
source("R-scripts/Source scripts//4.Process the CLs and DFs.R")

# Export the cleaning logs
source("R-scripts/Source scripts//5. Export the logbooks.R")

# Export coverage update for partners 
source("R-scripts/Source scripts//Coverage update for partners.R")

# Export the price collected tracker
source("R-scripts/Source scripts/Data collection tracker.R")


# Generate the cleaning log of arabic values for the translation
source("R-scripts/Source scripts//6.Generate and export the cleaning log for Arabic translation.R")



# --------------------------------------------------------------------------------------------------------------
# REVIEW OF THE CLEANING
# --------------------------------------------------------------------------------------------------------------

# path_logbook <- "inputs/All (Ongoing).xlsx"
# excel_sheets(path_logbook)
# 
# cleaning_log <- read_xlsx(path_logbook) |> filter(!is.na(change_type))
# raw_data <- data

# loading the cleaning log 
source("R-scripts/Source scripts//Loading cleaning log data.R")

# Review the cleaning
source("R-scripts/Source scripts//7. Review the cleaning log.R")

# Generate the clean dataset and review the cleaning
source("R-scripts/Source scripts//8.Generate the clean df and review the cleaning.R")               # If there are errors, export the file and share it with Shima. Once corrected, go back to step 7. Repeat until there is no error.

# Remove the unnecessary variables, PII, Generate enumerator tracker and variable tracker
source("R-scripts/Source scripts/9. Remove the unnecessary variables.R")

# Export the final logbook
source("R-scripts/Source scripts/10.Export the logbook.R")

# Vizualise the data (prices and stocks), if there are values that are still standing out, flag them and discuss with the Assessment team, repeat from step 6.
source("R-scripts/Source scripts/Visualize prices and stocks boxplots.R")


# Once completed, generate the enumerator tracker html file

# ----------------------------------------------------------------------------------------------------------------------

getwd()
# Delete the outputs file 
dir_delete("outputs/")

# Add a check for the interquartile range (To be done later)
# Make sure the "State other" Are included



# Voici l'approche à suivre pour copier les fichiers créés dans le sharepoint directement - je dois d'abord le faire pour les dossiers, ensuite pour les fichiers
for (file in dir_ls(recursive = T)){
  if (file %in% dir_ls("../../../../../ACTED/IMPACT SDN - General/02. Programme/SDN2303_JMMI/6. Data cleaning/2025-05 May/", recursive = T)){
    
    dir_copy()
    
    
  }
}


dir_ls("../../../../../ACTED/IMPACT SDN - General/02. Programme/SDN2303_JMMI/6. Data cleaning/2025-05 May/", recursive = T)









