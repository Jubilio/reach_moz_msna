# =============================================================================
# 03_analysis_engine.R — Aggregation & Statistical Analysis Engine (English)
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

#' Computes mean percentages and positive counts for target indicators across specified disaggregation groups
calculate_summary_table <- function(df, target_vars, group_vars = NULL, label_group = "Total") {
  df_sub <- df

  if (!is.null(group_vars) && length(group_vars) > 0) {
    # Filter NAs in grouping dimensions
    for (gv in group_vars) {
      if (gv %in% names(df_sub)) {
        df_sub <- df_sub[!is.na(df_sub[[gv]]) & nzchar(trimws(as.character(df_sub[[gv]]))), ]
      }
    }
    grouped_df <- df_sub %>% dplyr::group_by(dplyr::across(dplyr::all_of(group_vars)))
  } else {
    grouped_df <- df_sub %>% dplyr::mutate(Disaggregation = label_group) %>% dplyr::group_by(Disaggregation)
  }

  summary_res <- grouped_df %>%
    dplyr::summarise(
      Sample_Size = dplyr::n(),
      dplyr::across(
        dplyr::all_of(intersect(target_vars, names(df_sub))),
        list(
          mean = ~ round(mean(to_numeric(.), na.rm = TRUE) * 100, 1),
          count = ~ sum(to_numeric(.) == 1, na.rm = TRUE)
        ),
        .names = "{.col}__{.fn}"
      ),
      .groups = "drop"
    )

  return(summary_res)
}

#' Executes full Education and MSNI analysis across all standard disaggregation dimensions
run_full_analysis <- function(edu_loop, main_df, cfg) {
  message("--> [Analysis Engine] Generating summary tables across disaggregations...")

  pop_group_col <- if ("population_group" %in% names(main_df)) "population_group" else if ("disp_status" %in% names(main_df)) "disp_status" else NULL

  disagg_list <- list(
    National    = character(0),
    Admin1      = "admin1",
    Admin2      = c("admin1", "admin2"),
    DispStatus  = pop_group_col,
    HoHGender   = if ("hoh_gender" %in% names(main_df)) "hoh_gender" else NULL
  )
  disagg_list <- disagg_list[!sapply(disagg_list, is.null)]

  # 1. Education Analysis (Individual Child Level: ages 5-17) -----------------
  edu_school_age <- edu_loop %>% dplyr::filter(is_school_age == TRUE)

  edu_target_vars <- c(
    "is_out_of_school", "is_overage_2plus", "ece_participation", "early_primary_enrolment",
    "any_disruption", "disrupt_hazard", "disrupt_teacher", "disrupt_shelter", "disrupt_attack",
    "barrier_cost", "barrier_distance", "barrier_safety", "barrier_marriage_pregnancy",
    "barrier_documentation", "barrier_disability", "barrier_closed"
  )

  # Attach household-level disaggregation keys to edu_loop if missing
  candidate_disagg <- c("admin1", "admin2", pop_group_col, "hoh_gender")
  available_disagg <- intersect(candidate_disagg, names(main_df))
  join_cols <- setdiff(available_disagg, names(edu_school_age))

  if (length(join_cols) > 0) {
    edu_school_age <- edu_school_age %>%
      dplyr::left_join(
        main_df %>% dplyr::select(uuid, dplyr::all_of(join_cols)),
        by = "uuid"
      )
  }

  edu_results <- list()
  for (dname in names(disagg_list)) {
    gvars <- disagg_list[[dname]]
    edu_results[[paste0("Edu_", dname)]] <- calculate_summary_table(
      df = edu_school_age,
      target_vars = edu_target_vars,
      group_vars = gvars,
      label_group = dname
    )

    # Disaggregation crossed by Child Gender
    edu_results[[paste0("Edu_Gender_", dname)]] <- calculate_summary_table(
      df = edu_school_age,
      target_vars = edu_target_vars,
      group_vars = c(gvars, "gender"),
      label_group = paste0(dname, " by Gender")
    )

    # Disaggregation crossed by School-Age Category
    edu_results[[paste0("Edu_AgeCat_", dname)]] <- calculate_summary_table(
      df = edu_school_age,
      target_vars = edu_target_vars,
      group_vars = c(gvars, "school_age_cat"),
      label_group = paste0(dname, " by Age Category")
    )
  }

  # 2. MSNI & Sectoral LSG Analysis (Household Level) ------------------------
  msni_target_vars <- c(
    "msni_in_need", "msni_in_severe_need",
    "comp_foodsec_in_need", "comp_foodsec_in_severe_need",
    "comp_wash_in_need", "comp_wash_in_severe_need",
    "comp_edu_in_need", "comp_edu_in_severe_need",
    "comp_health_in_need", "comp_health_in_severe_need",
    "comp_snfi_in_need", "comp_snfi_in_severe_need",
    "comp_prot_in_need", "comp_prot_in_severe_need"
  )

  msni_results <- list()
  for (dname in names(disagg_list)) {
    gvars <- disagg_list[[dname]]
    msni_results[[paste0("MSNI_", dname)]] <- calculate_summary_table(
      df = main_df,
      target_vars = msni_target_vars,
      group_vars = gvars,
      label_group = dname
    )
  }

  # MSNI Severity Phase Distribution by Province (% Phase 1, Phase 2, Phase 3, Phase 4+)
  msni_dist <- main_df %>%
    dplyr::group_by(admin1, msni_score) %>%
    dplyr::summarise(Count = dplyr::n(), .groups = "drop_last") %>%
    dplyr::mutate(Percentage = round(Count / sum(Count) * 100, 1))

  message("--> [Analysis Engine] Summary tables calculated successfully!")
  return(list(
    education = edu_results,
    msni = msni_results,
    msni_distribution = msni_dist
  ))
}
