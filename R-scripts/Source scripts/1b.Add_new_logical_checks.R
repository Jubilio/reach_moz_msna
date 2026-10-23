cat("\014")

# =================================================================================================
# REACH MSNA 2026 Mozambique - 1b. Supplementary logical checks (tool-validated, genuinely new)
# -------------------------------------------------------------------------------------------------
# Purpose:
#   Adds logical checks that are NOT already enforced by the XLSForm's constraint or relevant
#   (skip logic) rules, and are NOT already covered by the existing logical-check workbook.
#
#   Every check in this script was validated against:
#     1. The XLSForm constraint column — to ensure the tool does not already block the pattern.
#     2. The XLSForm relevant column — to ensure both questions can co-exist in the same record.
#     3. The existing logical-check workbook — to avoid duplicating existing checks.
#
# Why these are useful:
#   Kobo validates each field individually on the device. Cross-field logical relationships
#   (e.g. FCS vs HHS, debt vs amount, HHS sub-questions) and profile-level anomalies
#   (all max FCS, only condiments) cannot be enforced at collection time and require
#   post-collection review by the field team.
#
# Run position: immediately after Script 1, before Script 3.
# Required objects: path_checks, logical_checks (from Script 1).
# =================================================================================================

if (exists("print_step", mode = "function")) {
  print_step("1b. Adding supplementary logical checks and exporting updated workbook")
} else {
  cat("\n1b. Adding supplementary logical checks and exporting updated workbook\n")
}

# --------------------------------------------------------------------------------------------------
# Helper: build one check row
# --------------------------------------------------------------------------------------------------

new_check <- function(dataset, check_id, description, check_to_perform, columns_to_clean) {
  tibble::tibble(
    dataset           = as.character(dataset),
    check_id          = as.character(check_id),
    description       = as.character(description),
    check_to_perform  = as.character(check_to_perform),
    columns_to_clean  = as.character(columns_to_clean)
  )
}

# ==================================================================================================
# 1. NEW HOUSEHOLD-LEVEL (Submissions) CHECKS
# ==================================================================================================
# Justification notes are included for each check. All checks below were verified to NOT be
# covered by constraint / relevant / required in the final Mozambique XLSForm.

new_checks_household <- dplyr::bind_rows(

  # -----------------------------------------------------------------------
  # WASH
  # -----------------------------------------------------------------------

  # Tool constraint: wash_sanitation_facility_sharing_n allows 0–30 (or 999).
  # A count of 0 or 1 with sharing_yn = "yes" is logically wrong:
  #   - 0 means no household shares (contradicts "yes")
  #   - 1 means only the respondent's own household (also contradicts sharing)
  # The tool does NOT enforce a minimum of 2 when sharing is reported.
  new_check(
    dataset          = "Submissions",
    check_id         = "HH_NEW_001",
    description      = "Sanitation facility sharing is reported as 'yes' but the number of households sharing is 0 or 1, which contradicts a shared facility. Double-check the number of sharing households. they are to count the household they are in as well",
    check_to_perform = 'wash_sanitation_facility_sharing_yn == "yes" & !is.na(suppressWarnings(as.numeric(wash_sanitation_facility_sharing_n))) & suppressWarnings(as.numeric(wash_sanitation_facility_sharing_n)) <= 1 & suppressWarnings(as.numeric(wash_sanitation_facility_sharing_n)) != 999',
    columns_to_clean = "wash_sanitation_facility_sharing_yn, wash_sanitation_facility_sharing_n"
  ),

  # -----------------------------------------------------------------------
  # Food security — FCS × HHS cross-check
  # -----------------------------------------------------------------------

  # No constraint or relevant links FCS to HHS. A household with acceptable food consumption
  # (FCS > 42) reporting severe hunger symptoms (HHS ≥ 5) is an unusual and suspicious
  # combination requiring field verification.
  new_check(
    dataset          = "Submissions",
    check_id         = "HH_NEW_002",
    description      = "FCS score is in the acceptable range (> 42) but the Household Hunger Scale score indicates severe hunger (≥ 5). These two indicators are inconsistent and require double checking with the RAs",
    check_to_perform = '!is.na(suppressWarnings(as.numeric(fsl_fcs_score))) & suppressWarnings(as.numeric(fsl_fcs_score)) > 42 & !is.na(suppressWarnings(as.numeric(fsl_hhs_score))) & suppressWarnings(as.numeric(fsl_hhs_score)) >= 5',
    columns_to_clean = "fsl_fcs_cereal, fsl_fcs_legumes, fsl_fcs_dairy, fsl_fcs_meat, fsl_fcs_veg, fsl_fcs_fruit, fsl_fcs_oil, fsl_fcs_sugar, fsl_fcs_condiments, fsl_rcsi_lessquality, fsl_rcsi_borrow, fsl_rcsi_mealsize, fsl_rcsi_mealadult, fsl_rcsi_mealnb, fsl_hhs_nofoodhh, fsl_hhs_nofoodhh_freq, fsl_hhs_sleephungry, fsl_hhs_sleephungry_freq, fsl_hhs_alldaynight, fsl_hhs_alldaynight_freq"
  ),

  # -----------------------------------------------------------------------
  # Food security — FCS profile anomalies
  # -----------------------------------------------------------------------

  # The tool constrains each food group to 0–7 individually.
  # All food groups simultaneously at 7 is technically valid per the tool
  # but an extremely unusual profile. Enumerators should confirm.
  new_check(
    dataset          = "Submissions",
    check_id         = "HH_NEW_003",
    description      = "All FCS food groups (cereal, legumes, dairy, meat, vegetables, fruit, oil, sugar) are reported at their maximum value of 7 days each. This full-score profile is very unusual. Please confirm with the RAs",
    check_to_perform = '!is.na(suppressWarnings(as.numeric(fsl_fcs_cereal))) & suppressWarnings(as.numeric(fsl_fcs_cereal)) == 7 & suppressWarnings(as.numeric(fsl_fcs_legumes)) == 7 & suppressWarnings(as.numeric(fsl_fcs_dairy)) == 7 & suppressWarnings(as.numeric(fsl_fcs_meat)) == 7 & suppressWarnings(as.numeric(fsl_fcs_veg)) == 7 & suppressWarnings(as.numeric(fsl_fcs_fruit)) == 7 & suppressWarnings(as.numeric(fsl_fcs_oil)) == 7 & suppressWarnings(as.numeric(fsl_fcs_sugar)) == 7',
    columns_to_clean = "fsl_fcs_cereal, fsl_fcs_legumes, fsl_fcs_dairy, fsl_fcs_meat, fsl_fcs_veg, fsl_fcs_fruit, fsl_fcs_oil, fsl_fcs_sugar"
  ),

  # All main nutrition food groups are 0 but condiments were consumed.
  # Households cannot subsist on condiments alone; this profile almost certainly
  # reflects a data entry or comprehension error.
  new_check(
    dataset          = "Submissions",
    check_id         = "HH_NEW_004",
    description      = "All main food groups (cereal, legumes, dairy, meat, vegetables, fruit, oil, sugar) are 0 days but condiments were consumed. A household cannot subsist on condiments alone. Please verify with RA on all FCS responses.",
    check_to_perform = '!is.na(suppressWarnings(as.numeric(fsl_fcs_cereal))) & suppressWarnings(as.numeric(fsl_fcs_cereal)) == 0 & suppressWarnings(as.numeric(fsl_fcs_legumes)) == 0 & suppressWarnings(as.numeric(fsl_fcs_dairy)) == 0 & suppressWarnings(as.numeric(fsl_fcs_meat)) == 0 & suppressWarnings(as.numeric(fsl_fcs_veg)) == 0 & suppressWarnings(as.numeric(fsl_fcs_fruit)) == 0 & suppressWarnings(as.numeric(fsl_fcs_oil)) == 0 & suppressWarnings(as.numeric(fsl_fcs_sugar)) == 0 & !is.na(suppressWarnings(as.numeric(fsl_fcs_condiments))) & suppressWarnings(as.numeric(fsl_fcs_condiments)) > 0',
    columns_to_clean = "fsl_fcs_condiments, fsl_fcs_cereal, fsl_fcs_legumes, fsl_fcs_dairy, fsl_fcs_meat, fsl_fcs_veg, fsl_fcs_fruit, fsl_fcs_oil, fsl_fcs_sugar"
  ),

  # -----------------------------------------------------------------------
  # Food security — HHS sub-question logical consistency
  # -----------------------------------------------------------------------

  # The three HHS questions are all independently required (no linking relevant between them).
  # However, there is a logical hierarchy:
  #   nofoodhh   = ever ran out of food
  #   sleephungry = went to bed hungry
  #   alldaynight = went without eating for a whole day
  # If the household never ran out of food (nofoodhh = "no"), they cannot have gone to bed
  # hungry (sleephungry = "yes"). The tool does not enforce this cross-question logic.
  new_check(
    dataset          = "Submissions",
    check_id         = "HH_NEW_005",
    description      = "The household reported that they went to bed hungry (sleephungry = yes) but also reported they never lacked food at home (nofoodhh = no). These responses are logically inconsistent. Please re-verify the HHS questions.",
    check_to_perform = 'fsl_hhs_sleephungry == "yes" & fsl_hhs_nofoodhh == "no"',
    columns_to_clean = "fsl_hhs_sleephungry, fsl_hhs_nofoodhh"
  ),

  # If the household went a whole day and night without eating (alldaynight = "yes"),
  # they must also have slept hungry. alldaynight = "yes" with sleephungry = "no"
  # is logically impossible; the tool does not prevent this.
  new_check(
    dataset          = "Submissions",
    check_id         = "HH_NEW_006",
    description      = "The household reported going without food for a whole day and night (alldaynight = yes) but reported that they never went to bed hungry (sleephungry = no). Going a whole day without eating implies sleeping hungry. Please re-verify the HHS questions.",
    check_to_perform = 'fsl_hhs_alldaynight == "yes" & fsl_hhs_sleephungry == "no"',
    columns_to_clean = "fsl_hhs_alldaynight, fsl_hhs_sleephungry"
  ),

  # -----------------------------------------------------------------------
  # Food security — FCS × LCSI cross-check
  # -----------------------------------------------------------------------

  # LCSI stress strategies 1–4 are always required (no relevant restricting them).
  # Choices include: yes, no_had_no_need, no_exhausted, not_applicable.
  # "no_had_no_need" means the household reports they had NO NEED to use that coping strategy.
  # If FCS is poor/borderline (≤ 35) but ALL four stress LCSI strategies are "no_had_no_need",
  # the household claims it has no reason to cope despite low food consumption.
  # The tool does not enforce any link between FCS and LCSI responses.
  new_check(
    dataset          = "Submissions",
    check_id         = "HH_NEW_007",
    description      = "FCS score is poor or borderline (≤ 35) but all four LCSI stress strategies are reported as 'no_had_no_need' (no need to cope). Poor food consumption and no perceived need to cope are inconsistent. please check with the RAs",
    check_to_perform = '!is.na(suppressWarnings(as.numeric(fsl_fcs_score))) & suppressWarnings(as.numeric(fsl_fcs_score)) <= 35 & !is.na(fsl_lcsi_stress1) & fsl_lcsi_stress1 == "no_had_no_need" & !is.na(fsl_lcsi_stress2) & fsl_lcsi_stress2 == "no_had_no_need" & !is.na(fsl_lcsi_stress3) & fsl_lcsi_stress3 == "no_had_no_need" & !is.na(fsl_lcsi_stress4) & fsl_lcsi_stress4 == "no_had_no_need"',
    columns_to_clean = "fsl_fcs_score, fsl_lcsi_stress1, fsl_lcsi_stress2, fsl_lcsi_stress3, fsl_lcsi_stress4"
  ),

  # -----------------------------------------------------------------------
  # Cash and markets
  # -----------------------------------------------------------------------

  # cm_debt_amount is shown only when cm_debt = "yes" (RELEVANT rule).
  # Its constraint allows any value >= 0 or = -999 (DNK code).
  # A debt amount of exactly 0 when debt is reported as "yes" is logically suspicious:
  # if debt exists, the amount should be > 0 or -999 (don't know).
  new_check(
    dataset          = "Submissions",
    check_id         = "HH_NEW_008",
    description      = "Household reported having debt (cm_debt = yes) but the recorded debt amount is zero. A debt of zero is contradictory. Verify the debt amount or correct the debt yes/no response. (Note: -999 = don't know is acceptable.)",
    check_to_perform = 'cm_debt == "yes" & !is.na(suppressWarnings(as.numeric(cm_debt_amount))) & suppressWarnings(as.numeric(cm_debt_amount)) == 0',
    columns_to_clean = "cm_debt, cm_debt_amount"
  ),

  # cm_savings_amount is required when certain savings types are selected.
  # Its constraint allows >= 0 or = -999. A savings amount of exactly 0 when
  # savings are reported as "yes" is suspicious (having savings of zero is contradictory).
  new_check(
    dataset          = "Submissions",
    check_id         = "HH_NEW_009",
    description      = "Household reported having savings (cm_savings = yes) and a savings amount of zero was recorded. A savings balance of zero contradicts having savings. Verify the savings amount or the yes/no response. (Note: -999 = don't know is acceptable.)",
    check_to_perform = 'cm_savings == "yes" & !is.na(suppressWarnings(as.numeric(cm_savings_amount))) & suppressWarnings(as.numeric(cm_savings_amount)) == 0',
    columns_to_clean = "cm_savings, cm_savings_amount"
  ),

  # -----------------------------------------------------------------------
  # Population type × displacement status cross-check
  # -----------------------------------------------------------------------

  # hoh_type = "refugee" is a select_one with no constraint linking it to dis_forced.
  # A refugee household that reports "no_never" or the long "no_my_household..." code for
  # dis_forced is logically inconsistent: refugee status implies prior forced displacement.
  # The tool has no relevant or constraint enforcing this cross-check.
  new_check(
    dataset          = "Submissions",
    check_id         = "HH_NEW_010",
    description      = "Household type is recorded as 'refugee' but displacement status indicates the household was NEVER forcibly displaced. Refugee status implies a prior forced displacement experience. Verify both hoh_type and dis_forced responses.",
    check_to_perform = 'hoh_type == "refugee" & !is.na(dis_forced) & dis_forced %in% c("no_never", "no_my_household_was_not_forced_to_leave_our_home_b")',
    columns_to_clean = "hoh_type, dis_forced"
  )

)


# ==================================================================================================
# 2. NEW ROSTER CHECKS
# ==================================================================================================

new_checks_roster <- dplyr::bind_rows(

  # ind_age_under1 is shown and required ONLY when ind_age = 0 (relevant rule).
  # The tool enforces the question appears, but cannot prevent "no" from being selected
  # when age is 0. If age is 0, the member is by definition under 1 year old; "no" is wrong.
  new_check(
    dataset          = "roster",
    check_id         = "ROS_NEW_001",
    description      = "Roster member age is recorded as 0 but the 'under 1 year old' flag is 'no'. A member aged 0 years is by definition under 1 year old. Please verify the age or correct the under-1 flag.",
    check_to_perform = '!is.na(suppressWarnings(as.numeric(ind_age))) & suppressWarnings(as.numeric(ind_age)) == 0 & !is.na(ind_age_under1) & ind_age_under1 == "no"',
    columns_to_clean = "ind_age, ind_age_under1"
  ),

  # ind_id_doc_reason is ONLY shown when hoh_type = "refugee" AND ind_id_doc = "no".
  # If ind_id_doc = "yes" (has a document), the reason question was never presented
  # to the enumerator. A filled reason field alongside "yes" indicates a data anomaly —
  # either a prior entry was not cleared or the data bypassed skip logic.
  new_check(
    dataset          = "roster",
    check_id         = "ROS_NEW_002",
    description      = "Roster member has a valid ID document (ind_id_doc = yes) but a reason for lacking documentation (ind_id_doc_reason) is also recorded. These responses are contradictory; the reason field should be empty when a document is present. Check whether a previous entry was not cleared.",
    check_to_perform = 'ind_id_doc == "yes" & !is.na(ind_id_doc_reason) & trimws(as.character(ind_id_doc_reason)) != ""',
    columns_to_clean = "ind_id_doc, ind_id_doc_reason"
  ),

  # Each WGQ domain is constrained to the standard response scale individually,
  # but there is NO constraint preventing all six domains from simultaneously reaching
  # "cannot_do_at_all" (maximum severity). This extreme combination across all functioning
  # domains simultaneously is clinically very rare; the pattern typically indicates
  # a data entry error or misunderstanding of the response scale.
  new_check(
    dataset          = "roster",
    check_id         = "ROS_NEW_003",
    description      = "All six Washington Group disability domains (vision, hearing, mobility, cognition, self-care, communication) are recorded as 'cannot do at all'. This extreme profile across every domain simultaneously is clinically very rare and likely indicates a response scale misunderstanding or data entry error. Please verify with the enumerator.",
    check_to_perform = '!is.na(wgq_vision) & wgq_vision == "cannot_do_at_all" & !is.na(wgq_hearing) & wgq_hearing == "cannot_do_at_all" & !is.na(wgq_mobility) & wgq_mobility == "cannot_do_at_all" & !is.na(wgq_cognition) & wgq_cognition == "cannot_do_at_all" & !is.na(wgq_self_care) & wgq_self_care == "cannot_do_at_all" & !is.na(wgq_communication) & wgq_communication == "cannot_do_at_all"',
    columns_to_clean = "wgq_vision, wgq_hearing, wgq_mobility, wgq_cognition, wgq_self_care, wgq_communication"
  )

)


# ==================================================================================================
# 3. NEW EDU_IND CHECKS
# ==================================================================================================

new_checks_edu <- dplyr::bind_rows(

  # edu_disrupted_hazards, edu_disrupted_teacher, and edu_disrupted_displaced are all required
  # and shown ONLY when edu_access = "yes" (the child attends school). Each is independently
  # required = yes. There is no constraint preventing all three from being "yes" simultaneously
  # while the child still attends. A child simultaneously disrupted by all three mechanisms
  # while still attending is unusual and warrants verification of attendance vs. disruption.
  new_check(
    dataset          = "edu_ind",
    check_id         = "EDU_NEW_001",
    description      = "The child is recorded as attending school (edu_access = yes) but all three education disruption dimensions — hazards, teacher absence, and displacement — are simultaneously reported as 'yes'. Being disrupted by all three mechanisms while still attending is unusual. Please verify attendance and disruption responses.",
    check_to_perform = '!is.na(edu_access) & edu_access == "yes" & !is.na(edu_disrupted_hazards) & edu_disrupted_hazards == "yes" & !is.na(edu_disrupted_teacher) & edu_disrupted_teacher == "yes" & !is.na(edu_disrupted_displaced) & edu_disrupted_displaced == "yes"',
    columns_to_clean = "edu_access, edu_disrupted_hazards, edu_disrupted_teacher, edu_disrupted_displaced"
  )

)


# ==================================================================================================
# 4. NEW NUTRITION_IND CHECKS
# ==================================================================================================

new_checks_nutrition <- dplyr::bind_rows(

  # nut_iycf_proxy_available is required for under-2 children but its relevant does NOT gate
  # nut_iycf_proxy_foods or nut_iycf_proxy_bottle (both are simply shown for all under-2 children
  # regardless of proxy_available). If a caregiver/proxy is unavailable ("no"), responses to
  # the IYCF feeding questions are suspect: who provided the information?
  # This combination suggests a data collection protocol issue or enumerator error.
  new_check(
    dataset          = "nutrition_ind",
    check_id         = "NUT_NEW_001",
    description      = "IYCF proxy/caregiver was reported as unavailable (nut_iycf_proxy_available = no) but feeding responses (infant food groups or bottle use) are also recorded. If no caregiver was available to provide information. Please check with the enumerator who provided this information.",
    check_to_perform = 'nut_iycf_proxy_available == "no" & ((!is.na(nut_iycf_proxy_bottle) & trimws(as.character(nut_iycf_proxy_bottle)) != "") | (!is.na(nut_iycf_proxy_foods) & trimws(as.character(nut_iycf_proxy_foods)) != ""))',
    columns_to_clean = "nut_iycf_proxy_available, nut_iycf_proxy_foods, nut_iycf_proxy_bottle"
  )

)


# ==================================================================================================
# 4b. OUTLIER DETECTION CHECKS — Continuous variables and interview duration
# ==================================================================================================
# Purpose:
#   Automatically detect outliers in continuous (integer/decimal) responses and interview duration
#   using the IQR (Interquartile Range) method: flag values below Q1 - 1.5*IQR or above Q3 + 1.5*IQR.
#   This method is robust and data-driven, identifying genuinely unusual values for field verification.
#
# Datasets covered:
#   - Submissions: interview duration + all integer/decimal fields
#   - roster: age and other numeric indicators
#   - edu_ind, health_ind, nutrition_ind, child_protection_ind: numeric indicators
#
# Note: Outlier checks are built dynamically at runtime based on available data to handle
#       varying surveys and data formats. See the "Outlier Detection" section in Script 3
#       where these checks are evaluated against the actual cleaned data.
# ==================================================================================================

new_checks_outliers <- tibble::tibble(
  dataset           = character(),
  check_id          = character(),
  description       = character(),
  check_to_perform  = character(),
  columns_to_clean  = character()
)

# Interview duration outlier check (Submissions only)
# Detects interviews that are unusually short (< 5 minutes, likely skipped sections)
# or unusually long (> 3 hours, possible data entry delays or extended interviews)
new_checks_outliers <- dplyr::bind_rows(
  new_checks_outliers,
  new_check(
    dataset          = "Submissions",
    check_id         = "HH_OUTLIER_001",
    description      = "Interview duration is detected as an outlier using the IQR method. Unusually short interviews may indicate skipped sections; unusually long interviews may indicate data entry delays or extended interviews. Please verify the submission timestamps and interview quality.",
    check_to_perform = "is_duration_outlier == TRUE",  # Set dynamically in Script 3
    columns_to_clean = "start_time, end_time"
  )
)

# Template for continuous variable outliers (to be populated dynamically in Script 3)
# This ensures the cleaning log includes outlier checks even before running Script 3
new_checks_outliers <- dplyr::bind_rows(
  new_checks_outliers,
  new_check(
    dataset          = "Submissions",
    check_id         = "HH_OUTLIER_CONTINUOUS",
    description      = "One or more continuous (numeric integer/decimal) response values are detected as outliers using the IQR method (beyond 1.5 × IQR from quartiles). Outliers warrant field verification: they may indicate typos, unit misunderstandings, or genuine extreme cases. See the detailed cleaning log for which specific variables and values are flagged.",
    check_to_perform = "is_continuous_outlier == TRUE",  # Set dynamically in Script 3
    columns_to_clean = "[Populated dynamically in Script 3 based on detected outliers]"
  )
)

# Roster-level outlier check (age and numeric indicators)
new_checks_outliers <- dplyr::bind_rows(
  new_checks_outliers,
  new_check(
    dataset          = "roster",
    check_id         = "ROS_OUTLIER_001",
    description      = "A numeric response in the roster (e.g., age or other continuous field) is detected as an outlier using the IQR method. Please verify this value with the respondent.",
    check_to_perform = "is_outlier == TRUE",  # Set dynamically in Script 3
    columns_to_clean = "[Populated dynamically in Script 3 based on detected outliers]"
  )
)

# Indicator-level outlier checks (edu_ind, health_ind, nutrition_ind, child_protection_ind)
for (ind_dataset in c("edu_ind", "health_ind", "nutrition_ind", "child_protection_ind")) {
  new_checks_outliers <- dplyr::bind_rows(
    new_checks_outliers,
    new_check(
      dataset          = ind_dataset,
      check_id         = paste0(toupper(substring(ind_dataset, 1, 3)), "_OUTLIER_001"),
      description      = paste0("A numeric response in ", ind_dataset, " is detected as an outlier using the IQR method. Please verify this value."),
      check_to_perform = "is_outlier == TRUE",  # Set dynamically in Script 3
      columns_to_clean = "[Populated dynamically in Script 3 based on detected outliers]"
    )
  )
}


# ==================================================================================================
# 5. Merge into existing logical_checks list (in memory) and export updated workbook
# ==================================================================================================

merge_new_checks <- function(existing_list, dataset_key, new_rows) {
  existing <- existing_list[[dataset_key]]
  if (!is.data.frame(existing) || nrow(existing) == 0) {
    existing_list[[dataset_key]] <- new_rows
    return(existing_list)
  }
  # Idempotent: remove rows with the same check_id before appending
  existing_clean <- existing |>
    dplyr::filter(!check_id %in% new_rows$check_id)
  # Harmonise columns
  for (cc in setdiff(names(new_rows), names(existing_clean)))   existing_clean[[cc]] <- NA_character_
  for (cc in setdiff(names(existing_clean), names(new_rows)))   new_rows[[cc]]        <- NA_character_
  existing_list[[dataset_key]] <- dplyr::bind_rows(existing_clean, new_rows)
  existing_list
}

if (!exists("logical_checks") || !is.list(logical_checks)) {
  warning("'logical_checks' list not found; it will be built from the new checks only.", call. = FALSE)
  logical_checks <- list()
}

logical_checks <- merge_new_checks(logical_checks, "Submissions",          new_checks_household)
logical_checks <- merge_new_checks(logical_checks, "roster",               new_checks_roster)
logical_checks <- merge_new_checks(logical_checks, "edu_ind",              new_checks_edu)
logical_checks <- merge_new_checks(logical_checks, "nutrition_ind",        new_checks_nutrition)

# Merge outlier checks for all datasets
for (ds in unique(new_checks_outliers$dataset)) {
  logical_checks <- merge_new_checks(logical_checks, ds, 
                                     new_checks_outliers |> dplyr::filter(dataset == ds))
}

# Keep household_level alias consistent with Script 3's expectations
if ("household_level" %in% names(logical_checks)) {
  logical_checks$household_level <- logical_checks$Submissions
}

# Rebuild checks_all
expected_datasets_for_checks <- c(
  "Submissions", "roster", "edu_ind", "health_ind", "nutrition_ind", "child_protection_ind"
)
logical_checks$checks_all <- purrr::imap_dfr(
  logical_checks[names(logical_checks) %in% c(expected_datasets_for_checks, "cross_sheet")],
  ~ .x
)

# --------------------------------------------------------------------------------------------------
# Export updated workbook
# --------------------------------------------------------------------------------------------------

sheet_name_map <- c(
  Submissions          = "household_level",
  roster               = "roster",
  edu_ind              = "edu_ind",
  health_ind           = "health_ind",
  nutrition_ind        = "nutrition_ind",
  child_protection_ind = "child_protection_ind"
)

checks_path_updated <- sub(
  "\\.xlsx$",
  paste0("_updated_", format(Sys.Date(), "%Y-%m-%d"), ".xlsx"),
  path_checks
)

wb <- openxlsx::createWorkbook()

# Copy non-check sheets from original workbook (README, QA_summary, etc.)
original_sheets <- tryCatch(readxl::excel_sheets(path_checks), error = function(e) character())
non_check_sheets <- setdiff(original_sheets, c(unname(sheet_name_map), "checks_all"))

for (ns in non_check_sheets) {
  raw_ns <- tryCatch(
    readxl::read_xlsx(path_checks, sheet = ns, .name_repair = "unique_quiet"),
    error = function(e) tibble::tibble()
  )
  if (nrow(raw_ns) > 0) {
    openxlsx::addWorksheet(wb, ns)
    openxlsx::writeData(wb, ns, raw_ns)
  }
}

# Write each check sheet; highlight new check rows in light-yellow
new_ids_by_dataset <- list(
  Submissions          = c(new_checks_household$check_id, new_checks_outliers$check_id[new_checks_outliers$dataset == "Submissions"]),
  roster               = c(new_checks_roster$check_id, new_checks_outliers$check_id[new_checks_outliers$dataset == "roster"]),
  edu_ind              = c(new_checks_edu$check_id, new_checks_outliers$check_id[new_checks_outliers$dataset == "edu_ind"]),
  health_ind           = new_checks_outliers$check_id[new_checks_outliers$dataset == "health_ind"],
  nutrition_ind        = c(new_checks_nutrition$check_id, new_checks_outliers$check_id[new_checks_outliers$dataset == "nutrition_ind"]),
  child_protection_ind = new_checks_outliers$check_id[new_checks_outliers$dataset == "child_protection_ind"]
)

highlight_style <- openxlsx::createStyle(fgFill = "#FFF2CC")

for (ds_key in expected_datasets_for_checks) {
  sheet_nm <- sheet_name_map[[ds_key]]
  df_out   <- logical_checks[[ds_key]]

  if (!is.data.frame(df_out) || nrow(df_out) == 0) {
    df_out <- tibble::tibble(
      dataset = character(), check_id = character(),
      description = character(), check_to_perform = character(), columns_to_clean = character()
    )
  }

  df_out <- df_out |>
    dplyr::select(
      dplyr::any_of(c("dataset","check_id","description","check_to_perform","columns_to_clean")),
      dplyr::everything()
    )

  openxlsx::addWorksheet(wb, sheet_nm)
  openxlsx::writeData(wb, sheet_nm, df_out)

  new_ids <- new_ids_by_dataset[[ds_key]]
  if (length(new_ids) > 0 && "check_id" %in% names(df_out)) {
    new_rows_idx <- which(df_out$check_id %in% new_ids) + 1L  # +1 for header
    if (length(new_rows_idx) > 0) {
      openxlsx::addStyle(
        wb, sheet_nm,
        style      = highlight_style,
        rows       = new_rows_idx,
        cols       = seq_len(ncol(df_out)),
        gridExpand = TRUE
      )
    }
  }
}

openxlsx::addWorksheet(wb, "checks_all")
openxlsx::writeData(
  wb, "checks_all",
  logical_checks$checks_all %||% tibble::tibble()
)

openxlsx::saveWorkbook(wb, checks_path_updated, overwrite = TRUE)

# --------------------------------------------------------------------------------------------------
# Summary
# --------------------------------------------------------------------------------------------------

new_check_counts <- tibble::tibble(
  dataset  = c("Submissions (household_level)", "roster", "edu_ind", "nutrition_ind", "Outliers (all datasets)"),
  new_checks_added = c(
    nrow(new_checks_household),
    nrow(new_checks_roster),
    nrow(new_checks_edu),
    nrow(new_checks_nutrition),
    nrow(new_checks_outliers)
  ),
  rationale = c(
    "Sanitation sharing count, FCS×HHS, FCS anomaly profiles (max/condiments-only), HHS sub-question hierarchy, FCS×LCSI, debt/savings amounts, refugee×displacement",
    "Age=0 vs under-1 flag, id_doc contradiction, all-WGQ-max profile",
    "All three disruptions yes while attending",
    "IYCF responses filled but proxy unavailable",
    "Interview duration + continuous variable outliers (IQR method) for all datasets"
  )
)

cat("\nNew logical checks added (genuinely not covered by tool constraints/skip logic):\n")
print(new_check_counts, n = Inf)
cat("\nTotal new checks:", sum(new_check_counts$new_checks_added), "\n")
cat("Updated logical checks workbook saved to:\n ", checks_path_updated, "\n\n")

if (exists("print_step", mode = "function")) {
  print_step("1b. Supplementary logical checks added and workbook exported")
} else {
  cat("Done: supplementary logical checks added and workbook exported.\n")
}


