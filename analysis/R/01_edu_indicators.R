# =============================================================================
# 01_edu_indicators.R — Education Analysis Engine (EduAnalysisTool & Humind)
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(humind)
})

#' Computes all individual and household-level education indicators
#' based on the EduAnalysisTool (martivit/IMPACT) and Humind framework.
compute_education_indicators <- function(edu_loop, main_df, cfg) {
  message("--> [EduAnalysisTool] Computing Education indicators...")

  x <- edu_loop

  # 1. Age and Schooling Eligibility -----------------------------------------
  x$age <- to_numeric(x$edu_ind_age)
  x$gender <- as.character(x$edu_ind_gender)

  age_min <- cfg$education$age_min
  age_max <- cfg$education$age_max

  x$is_school_age <- !is.na(x$age) & x$age >= age_min & x$age <= age_max

  # School-Age Categories (SNE Mozambique & EduAnalysisTool guidance)
  x$school_age_cat <- dplyr::case_when(
    !x$is_school_age ~ "not_school_age",
    x$age == cfg$education$ece_age ~ "5_years_ece",
    x$age >= cfg$education$primary_age_min & x$age <= cfg$education$primary_age_max ~ "primary_school_age",
    x$age >= cfg$education$lower_sec_age_min & x$age <= cfg$education$lower_sec_age_max ~ "lower_secondary_age",
    x$age >= cfg$education$upper_sec_age_min & x$age <= cfg$education$upper_sec_age_max ~ "upper_secondary_age",
    TRUE ~ NA_character_
  )

  # Schooling Age Flag for humind functions (1 if school-age, 0 otherwise)
  x$edu_ind_age_schooling <- ifelse(x$is_school_age, 1, 0)

  # 2. Access to Education and Out-of-School Children (OOS) ------------------
  x$access_raw <- as.character(x$edu_access)
  x$access_status <- dplyr::case_when(
    !x$is_school_age ~ NA_character_,
    x$access_raw == "yes" ~ "accessing_education",
    x$access_raw == "no"  ~ "out_of_school",
    TRUE ~ NA_character_
  )

  x$is_out_of_school <- dplyr::case_when(
    !x$is_school_age ~ NA_real_,
    x$access_status == "out_of_school" ~ 1,
    x$access_status == "accessing_education" ~ 0,
    TRUE ~ NA_real_
  )

  # Flags for humind aggregation
  x$edu_ind_access_d    <- ifelse(x$is_school_age & x$access_raw == "yes", 1, 0)
  x$edu_ind_no_access_d <- ifelse(x$is_school_age & x$access_raw == "no", 1, 0)

  # 3. ISCED Grade Mapping and Over-Age Attendance ----------------------------
  isced_map <- readr::read_csv(cfg$education$isced_mapping_path, show_col_types = FALSE)

  x <- x %>%
    dplyr::left_join(
      isced_map %>% dplyr::select(code, cycle, intended_age),
      by = c("edu_level_grade" = "code")
    )

  # Over-age attendance: at least 2 years older than intended age for grade
  x$age_difference <- ifelse(!is.na(x$intended_age), x$age - x$intended_age, NA_real_)
  x$is_overage_2plus <- dplyr::case_when(
    x$access_status != "accessing_education" ~ NA_real_,
    is.na(x$age_difference) ~ NA_real_,
    x$age_difference >= 2 ~ 1,
    TRUE ~ 0
  )

  # ECE Participation & Early Primary Enrolment (5-year-old children)
  x$ece_participation <- dplyr::case_when(
    x$school_age_cat != "5_years_ece" ~ NA_real_,
    x$access_status == "accessing_education" ~ 1,
    x$access_status == "out_of_school" ~ 0,
    TRUE ~ NA_real_
  )

  x$early_primary_enrolment <- dplyr::case_when(
    x$school_age_cat != "5_years_ece" ~ NA_real_,
    x$access_status == "accessing_education" & x$cycle == "primary" ~ 1,
    x$access_status == "accessing_education" & x$cycle != "primary" ~ 0,
    x$access_status == "out_of_school" ~ 0,
    TRUE ~ NA_real_
  )

  # 4. Education Disruption Events --------------------------------------------
  has_col <- function(df, cname) cname %in% names(df)

  x$disrupt_hazard <- if (has_col(x, "edu_disruption_events/natural_hazard")) {
    to_numeric(x[["edu_disruption_events/natural_hazard"]])
  } else 0

  x$disrupt_teacher <- if (has_col(x, "edu_disruption_events/teachers_absence")) {
    to_numeric(x[["edu_disruption_events/teachers_absence"]])
  } else 0

  x$disrupt_shelter <- if (has_col(x, "edu_disruption_events/school_used_as_shelter")) {
    to_numeric(x[["edu_disruption_events/school_used_as_shelter"]])
  } else 0

  x$disrupt_attack <- if (has_col(x, "edu_disruption_events/direct_attack_on_education")) {
    to_numeric(x[["edu_disruption_events/direct_attack_on_education"]])
  } else 0

  x$any_disruption <- ifelse(
    x$access_status == "accessing_education",
    as.numeric((x$disrupt_hazard == 1 | x$disrupt_teacher == 1 | x$disrupt_shelter == 1 | x$disrupt_attack == 1)),
    NA_real_
  )

  # Flags for humind
  x$edu_disrupted_attack_d    <- ifelse(x$access_status == "accessing_education" & x$disrupt_attack == 1, 1, 0)
  x$edu_disrupted_hazards_d   <- ifelse(x$access_status == "accessing_education" & x$disrupt_hazard == 1, 1, 0)
  x$edu_disrupted_displaced_d <- ifelse(x$access_status == "accessing_education" & x$disrupt_shelter == 1, 1, 0)
  x$edu_disrupted_teacher_d   <- ifelse(x$access_status == "accessing_education" & x$disrupt_teacher == 1, 1, 0)

  # 5. Barriers to Education (for Out-of-School Children) ----------------------
  x$barrier_cost <- if (has_col(x, "edu_barrier_accessing_education/lack_of_money")) {
    to_numeric(x[["edu_barrier_accessing_education/lack_of_money"]])
  } else 0

  x$barrier_distance <- if (has_col(x, "edu_barrier_accessing_education/distance_is_far")) {
    to_numeric(x[["edu_barrier_accessing_education/distance_is_far"]])
  } else 0

  x$barrier_safety <- if (has_col(x, "edu_barrier_accessing_education/unsafe_journey") ||
                          has_col(x, "edu_barrier_accessing_education/harassment_or_violence_otw_school")) {
    s1 <- if (has_col(x, "edu_barrier_accessing_education/unsafe_journey")) to_numeric(x[["edu_barrier_accessing_education/unsafe_journey"]]) else 0
    s2 <- if (has_col(x, "edu_barrier_accessing_education/harassment_or_violence_otw_school")) to_numeric(x[["edu_barrier_accessing_education/harassment_or_violence_otw_school"]]) else 0
    as.numeric(s1 == 1 | s2 == 1)
  } else 0

  x$barrier_marriage_pregnancy <- if (has_col(x, "edu_barrier_accessing_education/early_marriage_pregnancy")) {
    to_numeric(x[["edu_barrier_accessing_education/early_marriage_pregnancy"]])
  } else 0

  x$barrier_documentation <- if (has_col(x, "edu_barrier_accessing_education/lack_of_birth_certificate")) {
    to_numeric(x[["edu_barrier_accessing_education/lack_of_birth_certificate"]])
  } else 0

  x$barrier_disability <- if (has_col(x, "edu_barrier_accessing_education/disability_or_health_condition")) {
    to_numeric(x[["edu_barrier_accessing_education/disability_or_health_condition"]])
  } else 0

  x$barrier_closed <- if (has_col(x, "edu_barrier_accessing_education/school_is_closed")) {
    to_numeric(x[["edu_barrier_accessing_education/school_is_closed"]])
  } else 0

  # Protection barrier flag for Education composite
  x$edu_ind_barrier_protection_d <- ifelse(
    x$access_status == "out_of_school" &
      (x$barrier_safety == 1 | x$barrier_marriage_pregnancy == 1 | x$barrier_documentation == 1),
    1, 0
  )

  # 6. Household-Level Aggregation (Submissions) -------------------------------
  message("--> [Humind] Aggregating education indicators to household level...")

  hh_edu <- x %>%
    dplyr::group_by(uuid) %>%
    dplyr::summarise(
      edu_schooling_age_n    = sum(is_school_age, na.rm = TRUE),
      edu_access_n           = sum(edu_ind_access_d == 1, na.rm = TRUE),
      edu_no_access_n        = sum(edu_ind_no_access_d == 1, na.rm = TRUE),
      edu_barrier_protection_n = sum(edu_ind_barrier_protection_d == 1, na.rm = TRUE),
      edu_disrupted_attack_n   = sum(edu_disrupted_attack_d == 1, na.rm = TRUE),
      edu_disrupted_hazards_n  = sum(edu_disrupted_hazards_d == 1, na.rm = TRUE),
      edu_disrupted_displaced_n= sum(edu_disrupted_displaced_d == 1, na.rm = TRUE),
      edu_disrupted_teacher_n  = sum(edu_disrupted_teacher_d == 1, na.rm = TRUE),
      .groups = "drop"
    )

  # Join back to household dataset (Submissions)
  main_df <- main_df %>%
    dplyr::left_join(hh_edu, by = "uuid")

  # Fill NAs with 0 for households with no school-age children in the loop
  main_df$edu_schooling_age_n[is.na(main_df$edu_schooling_age_n)] <- 0
  main_df$edu_access_n[is.na(main_df$edu_access_n)] <- 0
  main_df$edu_no_access_n[is.na(main_df$edu_no_access_n)] <- 0
  main_df$edu_barrier_protection_n[is.na(main_df$edu_barrier_protection_n)] <- 0
  main_df$edu_disrupted_attack_n[is.na(main_df$edu_disrupted_attack_n)] <- 0
  main_df$edu_disrupted_hazards_n[is.na(main_df$edu_disrupted_hazards_n)] <- 0
  main_df$edu_disrupted_displaced_n[is.na(main_df$edu_disrupted_displaced_n)] <- 0
  main_df$edu_disrupted_teacher_n[is.na(main_df$edu_disrupted_teacher_n)] <- 0

  # Compute Humind Education Composite (comp_edu_score: 1 to 4+)
  main_df <- humind::add_comp_edu(
    main_df,
    schooling_age_n       = "edu_schooling_age_n",
    no_access_n           = "edu_no_access_n",
    barrier_protection_n  = "edu_barrier_protection_n",
    attack_n              = "edu_disrupted_attack_n",
    hazards_n             = "edu_disrupted_hazards_n",
    displaced_n           = "edu_disrupted_displaced_n",
    teacher_n             = "edu_disrupted_teacher_n"
  )

  return(list(edu_loop = x, main = main_df))
}
