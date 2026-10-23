# =============================================================================
# Mozambique MSNA 2026 — Integrated Analysis Pipeline (English)
# EduAnalysisTool (martivit/IMPACT) & Humind MSNI Workflow (IMPACT Initiatives)
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(openxlsx)
  library(here)
  library(humind)
  library(fs)
  library(rmarkdown)
})

message("==================================================================")
message("   MOZAMBIQUE MSNA 2026 — INTEGRATED ANALYSIS PIPELINE (EDU & MSNI)")
message("==================================================================")

# 1. Load Configurations and Helper Modules ----------------------------------
source(here::here("analysis", "config", "config.R"))
source(here::here("analysis", "R", "00_utils.R"))
source(here::here("analysis", "R", "01_edu_indicators.R"))
source(here::here("analysis", "R", "02_msni_indicators.R"))
source(here::here("analysis", "R", "03_analysis_engine.R"))
source(here::here("analysis", "R", "04_export_outputs.R"))

# 2. Load Most Recent Clean Data from Mozambique Cleaning Pipeline -----------
data_tables <- load_latest_clean_data(outputs_dir = cfg$moz_outputs_dir)
main_raw    <- data_tables$Submissions
edu_raw     <- data_tables$edu_ind
health_raw  <- data_tables$health_ind

message("Loaded records:")
message("  - Households (Submissions): ", nrow(main_raw), " records")
message("  - Individual Education (edu_ind): ", nrow(edu_raw), " records")
if (!is.null(health_raw)) {
  message("  - Individual Health (health_ind): ", nrow(health_raw), " records")
}

# 3. Compute Education Indicators (EduAnalysisTool & Humind) ------------------
edu_out <- compute_education_indicators(
  edu_loop = edu_raw,
  main_df  = main_raw,
  cfg      = cfg
)
edu_processed <- edu_out$edu_loop
main_with_edu <- edu_out$main

# 4. Compute Sectoral LSG Composites & Overall MSNI (Humind MSNI) ------------
main_processed <- compute_msni_indicators(
  main_df     = main_with_edu,
  health_loop = health_raw,
  cfg         = cfg
)

# 5. Execute Statistical Analysis Engine (Disaggregations) -------------------
analysis_results <- run_full_analysis(
  edu_loop = edu_processed,
  main_df  = main_processed,
  cfg      = cfg
)

# 6. Export Results to Excel (.xlsx), HTML Report & RDS -----------------------
exports <- export_analysis_results(
  analysis_results = analysis_results,
  main_df          = main_processed,
  edu_df           = edu_processed,
  cfg              = cfg
)

# 7. Print Executive Summary to Console ---------------------------------------
message("\n==================================================================")
message("                   EXECUTIVE ANALYSIS SUMMARY")
message("==================================================================")
nat_msni <- analysis_results$msni$MSNI_National
nat_edu  <- analysis_results$education$Edu_National

cat("SURVEYED HOUSEHOLDS: ", format(nat_msni$Sample_Size, big.mark = ","), "\n")
cat("SCHOOL-AGE CHILDREN (5-17): ", format(nat_edu$Sample_Size, big.mark = ","), "\n\n")

cat("MULTI-SECTOR NEEDS INDEX (MSNI):\n")
cat("  - Households in Need (Score >= 3): ", nat_msni$msni_in_need__mean, "%\n", sep = "")
cat("  - Households in Severe Need (Score >= 4): ", nat_msni$msni_in_severe_need__mean, "%\n\n", sep = "")

cat("SECTORAL LIVING STANDARDS GAPS (% IN NEED >= 3):\n")
cat("  - Food Security & Livelihoods (FSL LSG): ", nat_msni$comp_foodsec_in_need__mean, "%\n", sep = "")
cat("  - Water, Sanitation & Hygiene (WASH LSG): ", nat_msni$comp_wash_in_need__mean, "%\n", sep = "")
cat("  - Education (Edu LSG): ", nat_msni$comp_edu_in_need__mean, "%\n", sep = "")
cat("  - Health (Health LSG): ", nat_msni$comp_health_in_need__mean, "%\n", sep = "")
cat("  - Shelter & Non-Food Items (SNFI LSG): ", nat_msni$comp_snfi_in_need__mean, "%\n", sep = "")
cat("  - Protection (Prot LSG): ", nat_msni$comp_prot_in_need__mean, "%\n\n", sep = "")

cat("KEY EDUCATION FINDINGS (EduAnalysisTool):\n")
cat("  - Out-of-School Children Rate (OOS): ", nat_edu$is_out_of_school__mean, "%\n", sep = "")
cat("  - Over-Age Attendance (>= 2 years older than grade): ", nat_edu$is_overage_2plus__mean, "%\n", sep = "")
cat("  - Recent Education Disruption (Any Event): ", nat_edu$any_disruption__mean, "%\n", sep = "")
cat("  - Financial Costs Barrier: ", nat_edu$barrier_cost__mean, "%\n", sep = "")
cat("  - Distance to School Barrier: ", nat_edu$barrier_distance__mean, "%\n", sep = "")
cat("  - Security / Protection on Journey Barrier: ", nat_edu$barrier_safety__mean, "%\n")

message("==================================================================")
message("Generated Deliverables:")
message("  - Excel Workbook (Labeled): ", exports$xlsx)
message("  - Interactive HTML Report:  ", exports$html)
message("  - Enriched RDS Dataset:     ", exports$rds)
message("==================================================================")
