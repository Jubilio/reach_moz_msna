# =================================================================================================
# REACH MSNA 2026 Mozambique - Master cleaning script
# =================================================================================================

rm(list = ls())
Sys.setlocale("LC_TIME", "English")
options(scipen = 999)


# ---- Load packages ----
required_packages <- c("tidyverse", "readxl", "openxlsx", "writexl", "lubridate", "fs", "crayon", "janitor", "stringr", "purrr", "rlang", "tidyselect", "naniar")
optional_packages <- c("cleaninsgtools", "humind")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) stop("Please install required package: ", pkg, call. = FALSE)
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}
for (pkg in optional_packages) {
  if (requireNamespace(pkg, quietly = TRUE)) suppressPackageStartupMessages(library(pkg, character.only = TRUE)) else message("Optional package not installed: ", pkg, ". Local fallback functions will be used where possible.")
}


# Load necessary libraries and functions
library(tidyverse)
library(openxlsx)
library(writexl)
library(readxl)
library(naniar)
library(purrr)

# library(supporteR)
library(cleaningtools)
library(lubridate)
library(fs)
library(crayon)
source("R-scripts/Source scripts/my_functions.R")

source("R-scripts/Source scripts/Data cleaning - My functions.R")


# ---- Project folders ----
folders <- c("inputs", "inputs/feedback", "outputs", "R-scripts", "R-scripts/Source scripts", "archive")
for (folder in folders) if (!fs::dir_exists(folder)) fs::dir_create(folder)

path_cleaning_folder_today <- file.path("outputs", paste0("MSNA2026 Mozambique data cleaning ", format(Sys.Date(), format = "%Y-%b-%d")))
if (!fs::dir_exists(path_cleaning_folder_today)) fs::dir_create(path_cleaning_folder_today)

# ---- Input paths: edit only these if your file names differ ----
path_tool   <- "inputs/kobo-used.xlsx"

# withouth kampala
# path_data   <- "inputs/REACH_MOZ_MSNA2026_Kobo_form_-_all_versions_-_False_-_2026-09-21-08-19-51.xlsx"

path_data   <- "inputs/REACH_MOZ_MSNA2026_Kobo_form_-_all_versions_-_False_-_2026-10-05-06-13-42.xlsx"


path_checks <- "inputs/logical_checks.xlsx"


# Optional: string pattern for test submissions to remove. Set to NULL to keep all submissions.
test_submission_user_pattern <- NULL

# Cleaning-log review file(s) should be placed in inputs/feedback/ after the assessment team completes review.
# The scripts will load every .xlsx file in inputs/feedback/ that contains a cleaning_log/combined_log sheet.

# print_step("Starting REACH MSNA 2026 Mozambique cleaning pipeline")


# 1. Load and process raw Kobo export sheets
source("R-scripts/Source scripts/1.Load and process the data.R")

# 1b. Add supplementary logical checks and export updated logical checks workbook
source("R-scripts/Source scripts/1b.Add_new_logical_checks.R")

# 1b. 1b_outlier_detection_helpers
source("R-scripts/Source scripts/1b_outlier_detection_helpers.R")


# 2. Soft duplicate checks on main household submissions
source("R-scripts/Source scripts/2.Soft duplicate checks.R")



# 3. Run checklist-aligned and logical checks on all export sheets/repeats
source("R-scripts/Source scripts/3.Do the checks.R")

admin2_export_summary

# 4. Process cleaning logs and dashboard summaries
source("R-scripts/Source scripts/4.Process the CLs and DFs.R")

# Generating quality analysis
# source("7.Generate_quality_analysis.R")

# 5. Export logbooks for assessor review
source("R-scripts/Source scripts/5. Export the logbooks.R")


# 7b. exporting progress report
source("R-scripts/Source scripts/7.Target_progress_and_daily_rate_tracker.R")


# 6. Generate free-text / specify other-review logs
source("R-scripts/Source scripts/6.Specify_other_checks.R")



# 6. Generate free-text / translation-review logs
# source("R-scripts/Source scripts/6.Generate and export the cleaning log for translation.R")




# 8. Data collection tracker
source("R-scripts/Source scripts/Data collection tracker.R")


# 10. Generate daily RMarkdown field data collection tracker
# Place this after Script 5 export and before/around the Shiny cache step.
source("R-scripts/Source scripts/Generate_MSNA2026_Field_Data_Collection_Tracker.R")


# 9. Save Shiny app data cache
source("R-scripts/Source scripts/Save_MSNA2026_Shiny_inputs_cache.R")




#### working on the clening logs 

table(data$fsl_lcsi_stress_1)







# --------------------------------------------------------------------------------------------------------------
# REVIEW OF THE CLEANING
# --------------------------------------------------------------------------------------------------------------
# Place reviewed logbook(s) in inputs/feedback/ and run the remaining scripts.

source('R-scripts/Source scripts/Loading cleaning log data.R')
# source("R-scripts/Source scripts/7. Review the cleaning log.R")
# source("R-scripts/Source scripts/8.Generate the clean df and review the cleaning.R")
# source("R-scripts/Source scripts/computing_weights.R")
# source("R-scripts/Source scripts/9. Remove the unnecessary variables.R")
# source("R-scripts/Source scripts/10.Export the logbook.R")
# 
# print_step("MSNA 2026 Mozambique cleaning pipeline completed")
# getwd()
