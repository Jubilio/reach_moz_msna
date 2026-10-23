# =============================================================================
# 02_msni_indicators.R — Sectoral Living Standards Gaps (LSGs) and MSNI Engine
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(humind)
})

#' Calcula os compósitos setoriais (LSG) e o Multi-Sector Needs Index (MSNI)
#' de acordo com o framework oficial do IMPACT / Humind.
compute_msni_indicators <- function(main_df, health_loop, cfg) {
  message("--> [Humind MSNI] Calculando compósitos setoriais e índice MSNI...")

  x <- main_df

  # 1. Segurança Alimentar (Food Security LSG) --------------------------------
  message("    -> Compósito de Segurança Alimentar (comp_foodsec_score)...")

  # Mapear e limpar FCS
  x$fsl_fcs_cereal  <- mask_days(x$m3269_hh_fcs_staples)
  x$fsl_fcs_legumes <- mask_days(x$m3270_hh_fcs_pulses)
  x$fsl_fcs_dairy   <- mask_days(x$m3271_hh_fcs_dairy)
  x$fsl_fcs_meat    <- mask_days(x$m3272_hh_fcs_protein)
  x$fsl_fcs_veg     <- mask_days(x$m3273_hh_fcs_veg)
  x$fsl_fcs_fruit   <- mask_days(x$m3274_hh_fcs_fruits)
  x$fsl_fcs_oil     <- mask_days(x$m3275_hh_fcs_fat)
  x$fsl_fcs_sugar   <- mask_days(x$m3276_hh_fcs_sugar)

  x <- humind::add_fcs(
    x,
    fsl_fcs_cereal  = "fsl_fcs_cereal",
    fsl_fcs_legumes = "fsl_fcs_legumes",
    fsl_fcs_dairy   = "fsl_fcs_dairy",
    fsl_fcs_meat    = "fsl_fcs_meat",
    fsl_fcs_veg     = "fsl_fcs_veg",
    fsl_fcs_fruit   = "fsl_fcs_fruit",
    fsl_fcs_oil     = "fsl_fcs_oil",
    fsl_fcs_sugar   = "fsl_fcs_sugar",
    cutoffs         = cfg$fsl$fcs_cutoffs
  )

  # Mapear e limpar rCSI
  x$fsl_rcsi_lessquality <- mask_days(x$fsl_rcsi_rely_on_less_expensive_food)
  x$fsl_rcsi_borrow      <- mask_days(x$fsl_rcsi_borrow)
  x$fsl_rcsi_mealsize    <- mask_days(x$fsl_rcsi_mealsize)
  x$fsl_rcsi_mealadult   <- mask_days(x$fsl_rcsi_mealadult)
  x$fsl_rcsi_mealnb      <- mask_days(x$fsl_rcsi_mealnb)

  x <- humind::add_rcsi(
    x,
    fsl_rcsi_lessquality = "fsl_rcsi_lessquality",
    fsl_rcsi_borrow      = "fsl_rcsi_borrow",
    fsl_rcsi_mealsize    = "fsl_rcsi_mealsize",
    fsl_rcsi_mealadult   = "fsl_rcsi_mealadult",
    fsl_rcsi_mealnb      = "fsl_rcsi_mealnb"
  )

  # Mapear e limpar HHS
  x$fsl_hhs_nofoodhh         <- as.character(x$m2754_hh_hhs_no_food)
  x$fsl_hhs_nofoodhh_freq    <- as.character(x$m2755_hh_hhs_no_food_frequency)
  x$fsl_hhs_sleephungry      <- as.character(x$m2756_hh_hhs_sleep_hungry)
  x$fsl_hhs_sleephungry_freq <- as.character(x$m2757_hh_hhs_sleep_hungry_frequency)
  x$fsl_hhs_alldaynight      <- as.character(x$m2758_hh_hhs_no_day_night)
  x$fsl_hhs_alldaynight_freq <- as.character(x$m2759_hh_hhs_no_day_night_frequency)

  x <- humind::add_hhs(
    x,
    fsl_hhs_nofoodhh         = "fsl_hhs_nofoodhh",
    fsl_hhs_nofoodhh_freq    = "fsl_hhs_nofoodhh_freq",
    fsl_hhs_sleephungry      = "fsl_hhs_sleephungry",
    fsl_hhs_sleephungry_freq = "fsl_hhs_sleephungry_freq",
    fsl_hhs_alldaynight      = "fsl_hhs_alldaynight",
    fsl_hhs_alldaynight_freq = "fsl_hhs_alldaynight_freq",
    yes_answer       = "yes",
    no_answer        = "no",
    rarely_answer    = "Rarely (1-2 times)",
    sometimes_answer = "Sometimes (3-10 times)",
    often_answer     = "Often (10+ times)"
  )

  # Fase FCM (Food Consumption Matrix)
  if (!"fsl_hdds_cat" %in% names(x)) x$fsl_hdds_cat <- NA_character_
  x <- humind::add_fcm_phase(x)

  # LCSI Moçambique
  lcsi_map <- readr::read_csv(cfg$fsl$lcsi_mapping_path, show_col_types = FALSE)
  lcsi_cols <- intersect(lcsi_map$variable, names(x))

  if (length(lcsi_cols) > 0) {
    # Calcular severidade máxima das estratégias utilizadas (código 30) ou esgotadas (20)
    lcsi_scores <- rep(0, nrow(x))
    for (i in seq_along(lcsi_cols)) {
      v <- lcsi_cols[i]
      sev <- lcsi_map$severity[lcsi_map$variable == v]
      ans <- as.character(x[[v]])
      used <- ans %in% c("30", "20")
      lcsi_scores[used] <- pmax(lcsi_scores[used], sev, na.rm = TRUE)
    }

    x$fsl_lcsi_score <- lcsi_scores
    x$fsl_lcsi_cat <- dplyr::case_when(
      lcsi_scores == 0 ~ "None",
      lcsi_scores == 1 ~ "Stress",
      lcsi_scores == 2 ~ "Crisis",
      lcsi_scores >= 3 ~ "Emergency",
      TRUE ~ "None"
    )

    x <- humind::add_fclcm_phase(x)
  }

  # Compósito de Segurança Alimentar (comp_foodsec_score)
  if ("fclcm_phase" %in% names(x)) {
    x <- humind::add_comp_foodsec(x)
  } else {
    # Fallback via FCM Phase caso FCLCM não esteja completo
    x$comp_foodsec_score <- dplyr::case_when(
      x$fsl_fcm_phase %in% c("Phase 4", "Phase 5") ~ 4,
      x$fsl_fcm_phase == "Phase 3" ~ 3,
      x$fsl_fcm_phase == "Phase 2" ~ 2,
      x$fsl_fcm_phase == "Phase 1" ~ 1,
      TRUE ~ 1
    )
    x$comp_foodsec_in_need <- as.numeric(x$comp_foodsec_score >= 3)
    x$comp_foodsec_in_severe_need <- as.numeric(x$comp_foodsec_score >= 4)
  }

  # 2. Água, Saneamento e Higiene (WASH LSG) ---------------------------------
  message("    -> Compósito de WASH (comp_wash_score)...")

  if ("wash_drinking_water_source" %in% names(x)) {
    x <- humind::add_drinking_water_source_cat(x)

    tc <- to_numeric(x$wash_drinking_water_time_int)
    time_cat <- dplyr::case_when(
      is.na(tc) ~ "under_30min",
      tc <= 30  ~ "under_30min",
      tc > 30   ~ "above_30min"
    )

    source_cat <- x$wash_drinking_water_source_cat

    # Escada JMP de Água
    x$wash_water_jmp <- dplyr::case_when(
      source_cat == "surface_water" ~ "surface_water",
      source_cat == "unimproved"    ~ "unimproved",
      source_cat == "improved" & time_cat == "above_30min" ~ "limited",
      source_cat == "improved" & time_cat == "under_30min" ~ "basic",
      TRUE ~ "unimproved"
    )

    # Escada JMP de Saneamento
    sanit <- as.character(x$wash_sanitation_facility)
    sharing <- as.character(x$wash_sanitation_facility_sharing_yn)

    x$wash_sanitation_jmp <- dplyr::case_when(
      sanit %in% c("none", "open_defecation", "bush") ~ "open_defecation",
      sanit %in% c("unimproved_pit_latrine", "hanging_toilet", "bucket") ~ "unimproved",
      sharing == "yes" ~ "limited",
      sharing == "no"  ~ "basic",
      TRUE ~ "limited"
    )

    # Compósito WASH (1 a 4+)
    x$comp_wash_score <- dplyr::case_when(
      x$wash_water_jmp == "surface_water" | x$wash_sanitation_jmp == "open_defecation" ~ 4,
      x$wash_water_jmp %in% c("unimproved", "limited") | x$wash_sanitation_jmp == "unimproved" ~ 3,
      x$wash_sanitation_jmp == "limited" ~ 2,
      TRUE ~ 1
    )
    x$comp_wash_in_need <- as.numeric(x$comp_wash_score >= 3)
    x$comp_wash_in_severe_need <- as.numeric(x$comp_wash_score >= 4)
  } else {
    x$comp_wash_score <- 1
    x$comp_wash_in_need <- 0
    x$comp_wash_in_severe_need <- 0
  }

  # 3. Saúde (Health LSG) ----------------------------------------------------
  message("    -> Compósito de Saúde (comp_health_score)...")

  if (!is.null(health_loop) && nrow(health_loop) > 0) {
    need <- as.character(health_loop$health_ind_healthcare_needed)
    rec  <- as.character(health_loop$health_ind_healthcare_received)

    health_loop$access_cat <- dplyr::case_when(
      need == "no" ~ "no_need",
      need == "yes" & rec == "yes" ~ "needed_met",
      need == "yes" & rec == "no"  ~ "needed_unmet",
      TRUE ~ "no_need"
    )

    hh_health <- health_loop %>%
      dplyr::group_by(uuid) %>%
      dplyr::summarise(
        health_ind_healthcare_needed_no_n        = sum(access_cat == "no_need", na.rm = TRUE),
        health_ind_healthcare_needed_yes_met_n   = sum(access_cat == "needed_met", na.rm = TRUE),
        health_ind_healthcare_needed_yes_unmet_n = sum(access_cat == "needed_unmet", na.rm = TRUE),
        .groups = "drop"
      )

    x <- x %>%
      dplyr::left_join(hh_health, by = "uuid")

    x$health_ind_healthcare_needed_no_n[is.na(x$health_ind_healthcare_needed_no_n)] <- 1
    x$health_ind_healthcare_needed_yes_met_n[is.na(x$health_ind_healthcare_needed_yes_met_n)] <- 0
    x$health_ind_healthcare_needed_yes_unmet_n[is.na(x$health_ind_healthcare_needed_yes_unmet_n)] <- 0

    x <- humind::add_comp_health(x)
  } else {
    x$comp_health_score <- 1
    x$comp_health_in_need <- 0
    x$comp_health_in_severe_need <- 0
  }

  # 4. Abrigo e Artigos Não Alimentares (SNFI LSG) ---------------------------
  message("    -> Compósito de Abrigo & NFI (comp_snfi_score)...")

  if ("snfi_shelter_damage" %in% names(x)) {
    dmg <- as.character(x$snfi_shelter_damage)
    stype <- as.character(x$snfi_shelter_type)

    severe_dmg <- grepl("major|severe|total|risk_collapse", dmg, ignore.case = TRUE)
    inadequate_type <- stype %in% c("makeshift_shelter", "tent", "open_air", "collective_centre")

    x$comp_snfi_score <- dplyr::case_when(
      severe_dmg | inadequate_type ~ 4,
      grepl("minor|damage", dmg, ignore.case = TRUE) ~ 3,
      stype %in% c("traditional_house", "unfinished") ~ 2,
      TRUE ~ 1
    )
    x$comp_snfi_in_need <- as.numeric(x$comp_snfi_score >= 3)
    x$comp_snfi_in_severe_need <- as.numeric(x$comp_snfi_score >= 4)
  } else {
    x$comp_snfi_score <- 1
    x$comp_snfi_in_need <- 0
    x$comp_snfi_in_severe_need <- 0
  }

  # 5. Proteção (Protection LSG) ----------------------------------------------
  message("    -> Compósito de Proteção (comp_prot_score)...")

  child_sep <- as.character(x$prot_child_sep) == "yes" | as.character(x$prot_hosting_child_sep) == "yes"
  child_labour <- to_numeric(x$prot_child_labour_n) > 0

  x$comp_prot_score <- dplyr::case_when(
    child_sep | child_labour ~ 4,
    as.character(x$prot_increase_violence_for_female) == "yes" ~ 3,
    as.character(x$prot_concern_hh_freq_violence) %in% c("always", "frequently") ~ 3,
    TRUE ~ 1
  )
  x$comp_prot_in_need <- as.numeric(x$comp_prot_score >= 3)
  x$comp_prot_in_severe_need <- as.numeric(x$comp_prot_score >= 4)

  # 6. Garantir Compósito de Educação -----------------------------------------
  if (!"comp_edu_score" %in% names(x)) {
    x$comp_edu_score <- 1
    x$comp_edu_in_need <- 0
    x$comp_edu_in_severe_need <- 0
  }

  # 7. Cálculo Geral do Multi-Sector Needs Index (MSNI) ----------------------
  message("    -> Agregação Global do MSNI (humind::add_msni)...")

  # Garantir tipo numérico em todos os compósitos
  sector_cols <- c("comp_foodsec_score", "comp_snfi_score", "comp_wash_score",
                   "comp_prot_score", "comp_health_score", "comp_edu_score")

  for (col in sector_cols) {
    x[[col]] <- to_numeric(x[[col]])
    x[[col]][is.na(x[[col]])] <- 1
  }

  x <- humind::add_msni(x)

  # Métricas adicionais de co-ocorrência multidimensional
  in_need_cols <- c("comp_foodsec_in_need", "comp_snfi_in_need", "comp_wash_in_need",
                    "comp_prot_in_need", "comp_health_in_need", "comp_edu_in_need")

  for (col in in_need_cols) {
    if (col %in% names(x)) x[[col]] <- to_numeric(x[[col]])
  }

  x$msni_sectors_in_need_n <- rowSums(x[, intersect(in_need_cols, names(x))], na.rm = TRUE)

  message("--> [Humind MSNI] Cálculo concluído com sucesso!")
  return(x)
}
