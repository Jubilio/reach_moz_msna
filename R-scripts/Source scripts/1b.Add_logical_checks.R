cat("\014")

# =================================================================================================
# REACH MSNA 2026 Mozambique - 1b. Add new logical checks to the logical checks workbook
# -------------------------------------------------------------------------------------------------
# Purpose:
#   Append additional logical checks to the existing checks workbook without modifying or
#   overwriting any previously defined checks.
#
# Structure:
#   - New checks are defined below per dataset (household_level, roster, edu_ind, health_ind,
#     nutrition_ind, child_protection_ind).
#   - IDs follow the existing naming convention (check_NN, ROS_NNN, EDU_NNN, ...).
#   - All checks are wrapped in suppressWarnings() or include NA guards where needed.
#   - After binding, the updated sheets are written back to the workbook using openxlsx,
#     preserving all other sheets (README, QA_summary, etc.).
#   - A dated backup of the original workbook is also written to outputs/.
#
# Run this script once to update the checks workbook. It is idempotent: checks already present
# (matched by check_id) will not be duplicated.
# =================================================================================================

if (!exists("path_checks")) {
  path_checks <- "inputs/logical_checks.xlsx"
}

required_for_checks <- c("dplyr", "tibble", "readxl", "openxlsx")
for (pkg in required_for_checks) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Package '", pkg, "' is required.", call. = FALSE)
  }
}

# --------------------------------------------------------------------------------------------------
# 0. Helper: read existing check sheet
# --------------------------------------------------------------------------------------------------

read_check_sheet <- function(path, sheet) {
  readxl::read_xlsx(path, sheet = sheet, .name_repair = "unique_quiet") |>
    janitor::clean_names() |>
    tibble::as_tibble() |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
}

# Canonical column order for every check sheet
check_col_order <- c("dataset", "check_id", "description", "check_to_perform", "columns_to_clean")

# --------------------------------------------------------------------------------------------------
# 1. Read existing checks
# --------------------------------------------------------------------------------------------------

existing_hh     <- read_check_sheet(path_checks, "household_level")
existing_roster <- read_check_sheet(path_checks, "roster")
existing_edu    <- read_check_sheet(path_checks, "edu_ind")
existing_health <- read_check_sheet(path_checks, "health_ind")
existing_nut    <- read_check_sheet(path_checks, "nutrition_ind")
existing_cp     <- read_check_sheet(path_checks, "child_protection_ind")

# --------------------------------------------------------------------------------------------------
# 2. New checks — household_level
# --------------------------------------------------------------------------------------------------
# Checks already present (check_1 … check_52). New IDs start from check_60 to leave room.
# All check expressions are written to be safe against NA values.

new_hh_checks <- tibble::tribble(
  ~dataset,           ~check_id,   ~description,                                                                                                                                         ~check_to_perform,                                                                                                                                                                                                                       ~columns_to_clean,

  "household_level",  "check_60",
  "Respondent is the HoH but the respondent gender differs from the reported HoH gender. The two should match when resp_hoh_yn = 'yes'. Please verify.",
  "resp_hoh_yn == 'yes' & !is.na(resp_gender) & !is.na(hoh_gender) & trimws(as.character(resp_gender)) != '' & trimws(as.character(hoh_gender)) != '' & as.character(resp_gender) != as.character(hoh_gender)",
  "resp_hoh_yn, resp_gender, hoh_gender",

  "household_level",  "check_61",
  "Water collection time is extremely high (more than 240 minutes / 4 hours). Please verify this value with the enumerator.",
  "!is.na(suppressWarnings(as.numeric(wash_drinking_water_time_int))) & suppressWarnings(as.numeric(wash_drinking_water_time_int)) > 240",
  "wash_drinking_water_time_int",

  "household_level",  "check_62",
  "Household says water is on the premises (in dwelling or plot), but also provides a non-zero collection time. These are contradictory. Please check.",
  "wash_drinking_water_time_yn %in% c('water_in_dwelling', 'water_in_plot') & !is.na(suppressWarnings(as.numeric(wash_drinking_water_time_int))) & suppressWarnings(as.numeric(wash_drinking_water_time_int)) > 0",
  "wash_drinking_water_time_yn, wash_drinking_water_time_int",

  "household_level",  "check_63",
  "All four HWISE water insecurity items are at the maximum level ('always'). This indicates extreme water insecurity. Please confirm with the enumerator.",
  "!is.na(wash_hwise_drink) & !is.na(wash_hwise_hands) & !is.na(wash_hwise_plans) & !is.na(wash_hwise_worry) & wash_hwise_drink == 'always' & wash_hwise_hands == 'always' & wash_hwise_plans == 'always' & wash_hwise_worry == 'always'",
  "wash_hwise_drink, wash_hwise_hands, wash_hwise_plans, wash_hwise_worry",

  "household_level",  "check_64",
  "Household reports open defecation (no sanitation facility) but also says it shares a sanitation facility with other households. These responses are contradictory.",
  "wash_sanitation_facility == 'no_facility_open_defecation' & wash_sanitation_facility_sharing_yn == 'yes'",
  "wash_sanitation_facility, wash_sanitation_facility_sharing_yn",

  "household_level",  "check_65",
  "Household size is 1 but the household also reports more than 0 children under 18 (ind_age_below18_n > 0). A single-person household cannot have children. Please verify.",
  "!is.na(suppressWarnings(as.numeric(hh_size))) & suppressWarnings(as.numeric(hh_size)) == 1 & !is.na(suppressWarnings(as.numeric(ind_age_below18_n))) & suppressWarnings(as.numeric(ind_age_below18_n)) > 0",
  "hh_size, ind_age_below18_n",

  "household_level",  "check_66",
  "Household reports using 'reduced meals for adults so children can eat' coping strategy (RCSI mealadult > 0), but household size is 1. There are no other adults or children to reduce meals for. Please verify.",
  "!is.na(suppressWarnings(as.numeric(fsl_rcsi_mealadult))) & suppressWarnings(as.numeric(fsl_rcsi_mealadult)) > 0 & !is.na(suppressWarnings(as.numeric(hh_size))) & suppressWarnings(as.numeric(hh_size)) == 1",
  "fsl_rcsi_mealadult, hh_size",

  "household_level",  "check_67",
  "FCS score is above the theoretical maximum of 112. This indicates an error in one or more food group consumption values. Please verify all FCS inputs.",
  "!is.na(suppressWarnings(as.numeric(fsl_fcs_score))) & suppressWarnings(as.numeric(fsl_fcs_score)) > 112",
  "fsl_fcs_cereal, fsl_fcs_legumes, fsl_fcs_dairy, fsl_fcs_meat, fsl_fcs_veg, fsl_fcs_fruit, fsl_fcs_oil, fsl_fcs_sugar",

  "household_level",  "check_68",
  "All livelihood coping strategies (stress, crisis and emergency) are reported as 'not_applicable' AND household size is greater than 5. Large households with no applicable coping strategies is unusual. Please verify.",
  "fsl_lcsi_stress1 == 'not_applicable' & fsl_lcsi_stress2 == 'not_applicable' & fsl_lcsi_stress3 == 'not_applicable' & fsl_lcsi_stress4 == 'not_applicable' & fsl_lcsi_crisis1 == 'not_applicable' & fsl_lcsi_crisis2 == 'not_applicable' & fsl_lcsi_crisis3 == 'not_applicable' & fsl_lcsi_emergency1 == 'not_applicable' & fsl_lcsi_emergency2 == 'not_applicable' & fsl_lcsi_emergency3 == 'not_applicable' & !is.na(suppressWarnings(as.numeric(hh_size))) & suppressWarnings(as.numeric(hh_size)) > 5",
  "fsl_lcsi_stress1, fsl_lcsi_stress2, fsl_lcsi_stress3, fsl_lcsi_stress4, fsl_lcsi_crisis1, fsl_lcsi_crisis2, fsl_lcsi_crisis3, fsl_lcsi_emergency1, fsl_lcsi_emergency2, fsl_lcsi_emergency3, hh_size",

  "household_level",  "check_69",
  "Household is using emergency-level coping strategies (selling last female animal or begging) but has an Acceptable FCS score (>56). Very high food consumption alongside emergency coping is inconsistent. Please verify.",
  "(!is.na(fsl_lcsi_emergency2) & fsl_lcsi_emergency2 == 'yes' | !is.na(fsl_lcsi_emergency3) & fsl_lcsi_emergency3 == 'yes') & !is.na(suppressWarnings(as.numeric(fsl_fcs_score))) & suppressWarnings(as.numeric(fsl_fcs_score)) > 56",
  "fsl_lcsi_emergency2, fsl_lcsi_emergency3, fsl_fcs_score",

  "household_level",  "check_70",
  "More than 10 separated children (boys + girls) are reported. This is an unusually high number. Please verify the count of separated children.",
  "prot_child_sep == 'yes' & !is.na(suppressWarnings(as.numeric(prot_child_sep_boys_n))) & !is.na(suppressWarnings(as.numeric(prot_child_sep_girls_n))) & (suppressWarnings(as.numeric(prot_child_sep_boys_n)) + suppressWarnings(as.numeric(prot_child_sep_girls_n))) > 10",
  "prot_child_sep, prot_child_sep_boys_n, prot_child_sep_girls_n",

  "household_level",  "check_71",
  "Household reports living in a permanent structure (house or apartment) but also reports severe or total shelter damage. This combination is unusual — please verify shelter type and damage.",
  "stringr::str_detect(tolower(as.character(snfi_shelter_type_individual)), 'house|apartment') & stringr::str_detect(tolower(as.character(snfi_shelter_damage)), 'severe|totally_destroyed')",
  "snfi_shelter_type_individual, snfi_shelter_damage",

  "household_level",  "check_72",
  "Household owns its shelter but reports difficulty sleeping due to lack of materials and also reports needing sleeping mats/bedding. Ownership is unusual with lack of sleeping materials. Please verify.",
  "hlp_occupancy == 'ownership' & stringr::str_detect(tolower(as.character(snfi_fds_sleeping_issue)), 'lack_materials') & stringr::str_detect(tolower(as.character(snfi_essential_items_missing)), 'sleep')",
  "hlp_occupancy, snfi_fds_sleeping_issue, snfi_essential_items_missing",

  "household_level",  "check_73",
  "Household has more than 10 people but zero children under 18 are reported. This is demographically unusual for Mozambique. Please verify the household composition.",
  "!is.na(suppressWarnings(as.numeric(hh_size))) & suppressWarnings(as.numeric(hh_size)) > 10 & !is.na(suppressWarnings(as.numeric(ind_age_below18_n))) & suppressWarnings(as.numeric(ind_age_below18_n)) == 0",
  "hh_size, ind_age_below18_n, ind_age_above18_n",

  "household_level",  "check_74",
  "Household income includes humanitarian assistance as a source but reports receiving no assistance in the past 12 months (aap_received_assistance_12m = 'no'). Please clarify.",
  "stringr::str_detect(tolower(as.character(cm_income_source)), 'humanitarian') & !is.na(aap_received_assistance_12m) & aap_received_assistance_12m == 'no'",
  "cm_income_source, aap_received_assistance_12m",

  "household_level",  "check_75",
  "Household reports spending on water (cm_expenditure_frequent_water > 0) but the drinking water source is a free public source (piped or borehole). Please verify whether the household really pays for water.",
  "!is.na(suppressWarnings(as.numeric(cm_expenditure_frequent_water))) & suppressWarnings(as.numeric(cm_expenditure_frequent_water)) > 0 & stringr::str_detect(tolower(as.character(wash_drinking_water_source)), 'piped_dwelling|borehole|protected_spring')",
  "cm_expenditure_frequent_water, wash_drinking_water_source"
)

# --------------------------------------------------------------------------------------------------
# 3. New checks — roster
# --------------------------------------------------------------------------------------------------

new_roster_checks <- tibble::tribble(
  ~dataset, ~check_id, ~description, ~check_to_perform, ~columns_to_clean,

  "roster", "ROS_013",
  "Household member gender is missing. Gender is a required field for all roster members.",
  "is.na(ind_gender) | trimws(as.character(ind_gender)) == ''",
  "ind_gender, ind_age",

  "roster", "ROS_014",
  "Household member is recorded as married and is also under 14 years old. Child marriage is concerning — please verify age and marital status.",
  "!is.na(suppressWarnings(as.numeric(ind_age))) & suppressWarnings(as.numeric(ind_age)) < 14 & !is.na(ind_civil_status) & ind_civil_status == 'married'",
  "ind_age, ind_civil_status",

  "roster", "ROS_015",
  "Washington Group disability questions are all missing for a household member aged 5 years or older, and at least one disability domain is expected. Please verify.",
  "!is.na(suppressWarnings(as.numeric(ind_age))) & suppressWarnings(as.numeric(ind_age)) >= 5 & is.na(wgq_vision) & is.na(wgq_hearing) & is.na(wgq_mobility) & is.na(wgq_cognition) & is.na(wgq_self_care) & is.na(wgq_communication)",
  "ind_age, wgq_vision, wgq_hearing, wgq_mobility, wgq_cognition, wgq_self_care, wgq_communication",

  "roster", "ROS_016",
  "At least one Washington Group disability question is answered 'a lot of difficulty' or 'cannot do at all' for a child under 5. WGQ-SS is only validated for ages 5 and above.",
  "!is.na(suppressWarnings(as.numeric(ind_age))) & suppressWarnings(as.numeric(ind_age)) < 5 & (stringr::str_detect(tolower(as.character(wgq_vision)), 'lot|cannot') | stringr::str_detect(tolower(as.character(wgq_hearing)), 'lot|cannot') | stringr::str_detect(tolower(as.character(wgq_mobility)), 'lot|cannot') | stringr::str_detect(tolower(as.character(wgq_cognition)), 'lot|cannot') | stringr::str_detect(tolower(as.character(wgq_self_care)), 'lot|cannot') | stringr::str_detect(tolower(as.character(wgq_communication)), 'lot|cannot'))",
  "ind_age, wgq_vision, wgq_hearing, wgq_mobility, wgq_cognition, wgq_self_care, wgq_communication",

  "roster", "ROS_017",
  "Household member is aged 0 years (newborn). If this is an infant under 1 year, please verify their age is correctly recorded and not an entry error.",
  "!is.na(suppressWarnings(as.numeric(ind_age))) & suppressWarnings(as.numeric(ind_age)) == 0",
  "ind_age",

  "roster", "ROS_018",
  "Individual has no ID document and the reason for lack of documentation is 'other', but the other-specify text for the reason is missing.",
  "ind_id_doc == 'no' & !is.na(ind_id_doc_reason) & ind_id_doc_reason == 'other' & (is.na(ind_id_doc_reason_other) | trimws(as.character(ind_id_doc_reason_other)) == '')",
  "ind_id_doc, ind_id_doc_reason, ind_id_doc_reason_other"
)

# --------------------------------------------------------------------------------------------------
# 4. New checks — edu_ind
# --------------------------------------------------------------------------------------------------

new_edu_checks <- tibble::tribble(
  ~dataset, ~check_id, ~description, ~check_to_perform, ~columns_to_clean,

  "edu_ind", "EDU_010",
  "Child is recorded as attending school (edu_access = yes) but ALL three disruption indicators (hazards, displaced, teacher absence) are also recorded as 'yes'. Attending school while facing all disruptions simultaneously is unusual. Please verify.",
  "edu_access == 'yes' & !is.na(edu_disrupted_hazards) & !is.na(edu_disrupted_displaced) & !is.na(edu_disrupted_teacher) & edu_disrupted_hazards == 'yes' & edu_disrupted_displaced == 'yes' & edu_disrupted_teacher == 'yes'",
  "edu_access, edu_disrupted_hazards, edu_disrupted_displaced, edu_disrupted_teacher",

  "edu_ind", "EDU_011",
  "Child is not attending school (edu_access = no) but disruption indicators are filled. If the child is not attending, disruption indicators should typically be blank or not applicable. Please verify.",
  "edu_access == 'no' & (!is.na(edu_disrupted_hazards) & trimws(as.character(edu_disrupted_hazards)) != '' | !is.na(edu_disrupted_displaced) & trimws(as.character(edu_disrupted_displaced)) != '' | !is.na(edu_disrupted_teacher) & trimws(as.character(edu_disrupted_teacher)) != '')",
  "edu_access, edu_disrupted_hazards, edu_disrupted_displaced, edu_disrupted_teacher",

  "edu_ind", "EDU_012",
  "Tertiary education is accessed (edu_tertiary_access = yes) but the child is younger than 17 years. Tertiary education for children under 17 is unusual. Please verify the age and education level.",
  "!is.na(suppressWarnings(as.numeric(edu_ind_age))) & suppressWarnings(as.numeric(edu_ind_age)) < 17 & !is.na(edu_tertiary_access) & edu_tertiary_access == 'yes'",
  "edu_ind_age, edu_tertiary_access",

  "edu_ind", "EDU_013",
  "Education barrier includes both 'distance / facility too far' and a protection-related barrier simultaneously. Please verify which is the primary barrier.",
  "!is.na(edu_barrier) & stringr::str_detect(tolower(as.character(edu_barrier)), 'distance|too_far') & stringr::str_detect(tolower(as.character(edu_barrier)), 'protection|violence|marriage|pregnancy|work')",
  "edu_barrier"
)

# --------------------------------------------------------------------------------------------------
# 5. New checks — health_ind
# --------------------------------------------------------------------------------------------------

new_health_checks <- tibble::tribble(
  ~dataset, ~check_id, ~description, ~check_to_perform, ~columns_to_clean,

  "health_ind", "HLT_010",
  "Healthcare was not needed (health_ind_healthcare_needed = no) but a healthcare provider location or type of care needed is filled. This is contradictory. Please verify.",
  "!is.na(health_ind_healthcare_needed) & health_ind_healthcare_needed == 'no' & (!is.na(health_ind_healthcare_received_location) & trimws(as.character(health_ind_healthcare_received_location)) != '' | !is.na(health_ind_healthcare_needed_type) & trimws(as.character(health_ind_healthcare_needed_type)) != '')",
  "health_ind_healthcare_needed, health_ind_healthcare_received_location, health_ind_healthcare_needed_type",

  "health_ind", "HLT_011",
  "Healthcare was received (health_ind_healthcare_received = yes) but the location includes 'did_not_seek_care'. These responses are contradictory — a person cannot both receive care and not seek care.",
  "!is.na(health_ind_healthcare_received) & health_ind_healthcare_received == 'yes' & !is.na(health_ind_healthcare_received_location) & stringr::str_detect(tolower(as.character(health_ind_healthcare_received_location)), 'did_not_seek|not_seek')",
  "health_ind_healthcare_received, health_ind_healthcare_received_location",

  "health_ind", "HLT_012",
  "Healthcare was needed but healthcare was not received (health_ind_healthcare_received = no), yet no barrier to healthcare access is provided. The reason for not seeking care is missing.",
  "!is.na(health_ind_healthcare_needed) & health_ind_healthcare_needed == 'yes' & !is.na(health_ind_healthcare_received) & health_ind_healthcare_received == 'no' & (is.na(health_ind_healthcare_received_location) | trimws(as.character(health_ind_healthcare_received_location)) == '')",
  "health_ind_healthcare_needed, health_ind_healthcare_received, health_ind_healthcare_received_location",

  "health_ind", "HLT_013",
  "Healthcare type includes both 'mental health' and 'routine checkup' simultaneously. Please verify whether these are distinct needs for the same individual in the same reference period.",
  "!is.na(health_ind_healthcare_needed_type) & stringr::str_detect(tolower(as.character(health_ind_healthcare_needed_type)), 'mental') & stringr::str_detect(tolower(as.character(health_ind_healthcare_needed_type)), 'routine|checkup')",
  "health_ind_healthcare_needed_type"
)

# --------------------------------------------------------------------------------------------------
# 6. New checks — nutrition_ind
# --------------------------------------------------------------------------------------------------

new_nut_checks <- tibble::tribble(
  ~dataset, ~check_id, ~description, ~check_to_perform, ~columns_to_clean,

  "nutrition_ind", "NUT_012",
  "MUAC measurement is outside the plausible range (< 70 mm or > 220 mm). This is likely a data entry error. Please verify the MUAC value.",
  "!is.na(suppressWarnings(as.numeric(nut_ind_under5_muac))) & (suppressWarnings(as.numeric(nut_ind_under5_muac)) < 70 | suppressWarnings(as.numeric(nut_ind_under5_muac)) > 220)",
  "nut_ind_under5_muac",

  "nutrition_ind", "NUT_013",
  "Child is reported as having bilateral oedema (nut_ind_under5_oedema = yes) but MUAC is in the normal range (> 125 mm). Oedema typically co-occurs with SAM. Please verify.",
  "!is.na(nut_ind_under5_oedema) & nut_ind_under5_oedema == 'yes' & !is.na(suppressWarnings(as.numeric(nut_ind_under5_muac))) & suppressWarnings(as.numeric(nut_ind_under5_muac)) > 125",
  "nut_ind_under5_oedema, nut_ind_under5_muac",

  "nutrition_ind", "NUT_014",
  "Child has MUAC indicating SAM (< 115 mm) but oedema is reported as 'no'. Please verify both the MUAC measurement and oedema assessment.",
  "!is.na(suppressWarnings(as.numeric(nut_ind_under5_muac))) & suppressWarnings(as.numeric(nut_ind_under5_muac)) < 115 & !is.na(nut_ind_under5_oedema) & nut_ind_under5_oedema == 'no'",
  "nut_ind_under5_muac, nut_ind_under5_oedema",

  "nutrition_ind", "NUT_015",
  "Proxy/caregiver for the nutrition module is reported as 'no proxy needed', but the child's age (nut_ind_age) is under 5 years. Young children always need a proxy respondent for the IYCF module. Please verify.",
  "!is.na(suppressWarnings(as.numeric(nut_ind_age))) & suppressWarnings(as.numeric(nut_ind_age)) < 5 & !is.na(nut_iycf_proxy_caregiver) & stringr::str_detect(tolower(as.character(nut_iycf_proxy_caregiver)), 'no_proxy|self')",
  "nut_ind_age, nut_iycf_proxy_caregiver",

  "nutrition_ind", "NUT_016",
  "Child was sick in the 2 weeks prior to data collection (nut_ind_under5_sick_2w = yes) but bilateral oedema is reported as unknown (dnk). If the child was recently sick, oedema status should be assessed. Please verify.",
  "!is.na(nut_ind_under5_sick_2w) & nut_ind_under5_sick_2w == 'yes' & !is.na(nut_ind_under5_oedema) & nut_ind_under5_oedema == 'dnk'",
  "nut_ind_under5_sick_2w, nut_ind_under5_oedema"
)

# --------------------------------------------------------------------------------------------------
# 7. New checks — child_protection_ind
# --------------------------------------------------------------------------------------------------

new_cp_checks <- tibble::tribble(
  ~dataset, ~check_id, ~description, ~check_to_perform, ~columns_to_clean,

  "child_protection_ind", "CP_005",
  "Child is reported as engaging in child labour (prot_child_labour = yes) but is under 5 years old. Children under 5 cannot perform hazardous or formal labour. Please verify the child's age and labour status.",
  "!is.na(suppressWarnings(as.numeric(prot_ind_age))) & suppressWarnings(as.numeric(prot_ind_age)) < 5 & !is.na(prot_child_labour) & prot_child_labour == 'yes'",
  "prot_ind_age, prot_child_labour",

  "child_protection_ind", "CP_006",
  "Child works at home (prot_child_work_hh = yes) but the number of hours worked per week is extremely high (> 70 hours). This may be a data entry error. Please verify.",
  "!is.na(prot_child_work_hh) & prot_child_work_hh == 'yes' & !is.na(suppressWarnings(as.numeric(prot_child_work_hh_hrs))) & suppressWarnings(as.numeric(prot_child_work_hh_hrs)) > 70",
  "prot_child_work_hh, prot_child_work_hh_hrs",

  "child_protection_ind", "CP_007",
  "Child protection repeat record is for a child under 3 years old. Most child protection indicators (work, labour) are not applicable for very young children. Please verify the child's age.",
  "!is.na(suppressWarnings(as.numeric(prot_ind_age))) & suppressWarnings(as.numeric(prot_ind_age)) < 3",
  "prot_ind_age",

  "child_protection_ind", "CP_008",
  "Child is reported as working outside the home (prot_child_labour = yes) but is under 10 years old. This is especially concerning — please verify the child's age and work status.",
  "!is.na(suppressWarnings(as.numeric(prot_ind_age))) & suppressWarnings(as.numeric(prot_ind_age)) < 10 & !is.na(prot_child_labour) & prot_child_labour == 'yes'",
  "prot_ind_age, prot_child_labour"
)

# --------------------------------------------------------------------------------------------------
# 8. Merge new checks with existing, deduplicating on check_id
# --------------------------------------------------------------------------------------------------

merge_checks <- function(existing_df, new_df) {
  required_cols <- c("dataset", "check_id", "description", "check_to_perform", "columns_to_clean")

  # Ensure all expected columns exist in both
  for (cc in required_cols) {
    if (!cc %in% names(existing_df)) existing_df[[cc]] <- NA_character_
    if (!cc %in% names(new_df))      new_df[[cc]]      <- NA_character_
  }

  existing_df <- existing_df |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
  new_df <- new_df |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

  # Only keep new rows whose check_id is not already in the existing file
  new_unique <- new_df |>
    dplyr::filter(!check_id %in% existing_df$check_id)

  if (nrow(new_unique) == 0) {
    message("All new checks already present in sheet — no rows added.")
    return(existing_df)
  }

  message("Adding ", nrow(new_unique), " new checks: ", paste(new_unique$check_id, collapse = ", "))

  dplyr::bind_rows(existing_df, new_unique) |>
    dplyr::select(dplyr::any_of(required_cols), dplyr::everything())
}

updated_hh     <- merge_checks(existing_hh,     new_hh_checks)
updated_roster <- merge_checks(existing_roster,  new_roster_checks)
updated_edu    <- merge_checks(existing_edu,     new_edu_checks)
updated_health <- merge_checks(existing_health,  new_health_checks)
updated_nut    <- merge_checks(existing_nut,     new_nut_checks)
updated_cp     <- merge_checks(existing_cp,      new_cp_checks)

# --------------------------------------------------------------------------------------------------
# 9. Write updated workbook — preserving all non-check sheets (README, QA_summary, etc.)
# --------------------------------------------------------------------------------------------------

# Backup the original checks file before modifying
backup_dir  <- "outputs"
if (!dir.exists(backup_dir)) dir.create(backup_dir, recursive = TRUE)
backup_path <- file.path(
  backup_dir,
  paste0("logical_checks_backup_", format(Sys.Date(), "%Y-%m-%d"), ".xlsx")
)

if (!file.exists(backup_path)) {
  file.copy(path_checks, backup_path, overwrite = FALSE)
  message("Backup written to: ", backup_path)
}

# Load the full original workbook to preserve other sheets (README, QA_summary)
wb <- openxlsx::loadWorkbook(path_checks)

# Helper to overwrite a sheet in place
overwrite_sheet <- function(wb, sheet_name, df) {
  if (sheet_name %in% names(wb)) {
    openxlsx::removeWorksheet(wb, sheet_name)
  }
  openxlsx::addWorksheet(wb, sheet_name)
  openxlsx::writeData(wb, sheet_name, df)

  # Freeze the header row and auto-fit columns
  openxlsx::freezePane(wb, sheet_name, firstRow = TRUE)
  openxlsx::setColWidths(wb, sheet_name, cols = seq_len(ncol(df)), widths = "auto")

  invisible(wb)
}

wb <- overwrite_sheet(wb, "household_level",      updated_hh)
wb <- overwrite_sheet(wb, "roster",               updated_roster)
wb <- overwrite_sheet(wb, "edu_ind",              updated_edu)
wb <- overwrite_sheet(wb, "health_ind",           updated_health)
wb <- overwrite_sheet(wb, "nutrition_ind",        updated_nut)
wb <- overwrite_sheet(wb, "child_protection_ind", updated_cp)

openxlsx::saveWorkbook(wb, path_checks, overwrite = TRUE)

cat("\nLogical checks workbook updated successfully: ", path_checks, "\n", sep = "")

# --------------------------------------------------------------------------------------------------
# 10. Summary of checks added
# --------------------------------------------------------------------------------------------------

checks_summary <- tibble::tribble(
  ~sheet,                 ~existing_checks,     ~new_checks_added,          ~total_checks,
  "household_level",      nrow(existing_hh),    nrow(updated_hh) - nrow(existing_hh),    nrow(updated_hh),
  "roster",               nrow(existing_roster),nrow(updated_roster) - nrow(existing_roster),  nrow(updated_roster),
  "edu_ind",              nrow(existing_edu),   nrow(updated_edu) - nrow(existing_edu),   nrow(updated_edu),
  "health_ind",           nrow(existing_health),nrow(updated_health) - nrow(existing_health),  nrow(updated_health),
  "nutrition_ind",        nrow(existing_nut),   nrow(updated_nut) - nrow(existing_nut),   nrow(updated_nut),
  "child_protection_ind", nrow(existing_cp),    nrow(updated_cp) - nrow(existing_cp),     nrow(updated_cp)
)

cat("\n")
print(checks_summary)
cat("\nTotal new checks added:", sum(checks_summary$new_checks_added), "\n")

# Reload so logical_checks list in the session is updated (if Script 1 has already run)
if (exists("logical_checks") && is.list(logical_checks)) {
  logical_checks$Submissions      <<- updated_hh
  logical_checks$household_level  <<- updated_hh
  logical_checks$roster           <<- updated_roster
  logical_checks$edu_ind          <<- updated_edu
  logical_checks$health_ind       <<- updated_health
  logical_checks$nutrition_ind    <<- updated_nut
  logical_checks$child_protection_ind <<- updated_cp

  logical_checks$checks_all <<- dplyr::bind_rows(
    updated_hh, updated_roster, updated_edu,
    updated_health, updated_nut, updated_cp
  )
  cat("In-session 'logical_checks' list updated.\n")
}
