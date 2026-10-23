# =============================================================================
# 02_msni_indicators.R — Sectoral Living Standards Gaps & MSNI Engine (English)
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(humind)
})

#' Computes sectoral composite indicators (LSGs) and the Multi-Sector Needs Index (MSNI)
#' according to the official IMPACT / Humind framework.
compute_msni_indicators <- function(main_df, health_loop, cfg) {
  message("--> [Humind MSNI] Computing sectoral composites and MSNI index...")

  x <- main_df

  # 1. Food Security & Livelihoods (FS LSG) -----------------------------------
  message("    -> Food Security Composite (comp_foodsec_score)...")

  # Map and sanitize FCS recall days
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

  # Map and sanitize rCSI recall days
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

  # Map and sanitize HHS questions
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

  # Food Consumption Matrix (FCM Phase)
  if (!"fsl_hdds_cat" %in% names(x)) x$fsl_hdds_cat <- NA_character_
  x <- humind::add_fcm_phase(x)

  # Mozambique LCSI Strategies
  lcsi_map <- readr::read_csv(cfg$fsl$lcsi_mapping_path, show_col_types = FALSE)
  lcsi_cols <- intersect(lcsi_map$variable, names(x))

  if (length(lcsi_cols) > 0) {
    # Calculate maximum severity of strategies used in the past 30 days (code 30) or exhausted (code 20)
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

  # Food Security Composite Score (comp_foodsec_score)
  if ("fclcm_phase" %in% names(x)) {
    x <- humind::add_comp_foodsec(x)
  } else {
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

  # 2. Water, Sanitation & Hygiene (WASH LSG) --------------------------------
  #    Uses the full official humind WASH function chain.
  #    IMPORTANT: skipped_drinking_water_source_premises reflects the

  #    Mozambique Kobo skip logic for wash_drinking_water_time_yn.
  #    Sources skipped because they are on premises:
  #      piped_dwelling, piped_compound, piped_neighbour
  #    Sources skipped for other reasons (dnk/pnta) are handled by
  #      skipped_drinking_water_source_undefined.
  message("    -> WASH Composite (comp_wash_score)...")

  if ("wash_drinking_water_source" %in% names(x)) {

    # --- 2a. Drinking Water Source Category (improved / unimproved / surface_water) ---
    x <- humind::add_drinking_water_source_cat(x)

    # --- 2b. Drinking Water Collection Time Category ---
    #  The Mozambique questionnaire does not export wash_drinking_water_time_yn;
    #  the skip logic hides it for on-premises and dnk/pnta sources.
    #  We reconstruct it from wash_drinking_water_source so that
    #  humind::add_drinking_water_time_cat() can properly classify on-premises
    #  sources as "premises" (not "undefined"), preventing denominator shifts.
    on_premises_sources <- c("piped_dwelling", "piped_compound", "piped_neighbour")
    undefined_sources   <- c("dnk", "pnta")

    src_val <- as.character(x$wash_drinking_water_source)
    # The valid values for wash_drinking_water_time_yn are:
    #   water_on_premises values (water_in_dwelling, water_in_plot),
    #   the number_minutes column name (meaning: use numeric minutes),
    #   dnk, and undefined (pnta).
    x$wash_drinking_water_time_yn <- dplyr::case_when(
      src_val %in% on_premises_sources ~ "water_in_dwelling",
      src_val %in% undefined_sources   ~ "pnta",
      TRUE                             ~ "wash_drinking_water_time_int"
    )

    # Ensure wash_drinking_water_time_int is numeric and named correctly
    x$wash_drinking_water_time_int <- to_numeric(x$wash_drinking_water_time_int)

    # Pre-create placeholder columns that add_drinking_water_time_cat checks
    # for existence before it runs (it writes to these columns as output).
    if (!"wash_drinking_water_time_sl" %in% names(x))
      x$wash_drinking_water_time_sl <- NA_character_

    x <- humind::add_drinking_water_time_cat(
      x,
      drinking_water_time_yn  = "wash_drinking_water_time_yn",
      water_on_premises       = c("water_in_dwelling", "water_in_plot"),
      number_minutes          = "wash_drinking_water_time_int",
      dnk                     = "dnk",
      undefined               = "pnta",
      drinking_water_time_int = "wash_drinking_water_time_int",
      max_minutes             = 600,
      drinking_water_time_sl  = "wash_drinking_water_time_sl",
      drinking_water_source   = "wash_drinking_water_source",
      skipped_drinking_water_source_premises = c("piped_dwelling", "piped_compound", "piped_neighbour"),
      skipped_drinking_water_source_undefined = c("dnk", "pnta")
    )

    # Convert 3-category time (under_30_min / 30min_1hr / more_than_1hr) into
    # the 2-category threshold (under_30min / above_30min) needed by JMP quality
    x <- humind::add_drinking_water_time_threshold_cat(
      x,
      drinking_water_time_30min_cat              = "wash_drinking_water_time_cat",
      drinking_water_time_30min_cat_premises     = "premises",
      drinking_water_time_30min_cat_under_30min  = "under_30_min",
      drinking_water_time_30min_cat_above_30min  = c("30min_1hr", "more_than_1hr"),
      drinking_water_time_30min_cat_undefined    = "undefined"
    )

    # --- 2c. Drinking Water Quality JMP Ladder ---
    x <- humind::add_drinking_water_quality_jmp_cat(x)

    # --- 2d. Sanitation: categorise facility, sharing, JMP ladder ---
    x <- humind::add_sanitation_facility_cat(
      x,
      sanitation_facility = "wash_sanitation_facility",
      improved    = c("flush_latrine", "pit_latrine_slab", "improved_pit_latrine",
                       "pit_latrine_with_chair", "chair_adapted_for_pwd"),
      unimproved  = c("pit_latrine_wo_slab", "open_hole", "emergency_communal_latrine"),
      none        = c("none", "open_defecation"),
      undefined   = c("other", "dnk", "pnta")
    )

    x <- humind::add_sharing_sanitation_facility_cat(
      x,
      sharing_sanitation_facility = "wash_sanitation_facility_sharing_yn",
      yes       = "yes",
      no        = "no",
      undefined = c("dnk", "pnta"),
      sanitation_facility         = "wash_sanitation_facility",
      skipped_sanitation_facility = c("none", "open_defecation")
    )

    x <- humind::add_sanitation_facility_jmp_cat(x)

    # --- 2e. Sharing Sanitation Facility N Individuals ---
    if ("wash_sanitation_facility_sharing_n" %in% names(x)) {
      # add_sharing_sanitation_facility_n_ind requires a weight column
      if (!"weight" %in% names(x)) x$weight <- 1
      # Ensure required columns are numeric
      x$wash_sanitation_facility_sharing_n <- to_numeric(x$wash_sanitation_facility_sharing_n)
      x$hh_size <- to_numeric(x$hh_size)
      x <- humind::add_sharing_sanitation_facility_n_ind(
        x,
        sanitation_facility_sharing_n = "wash_sanitation_facility_sharing_n",
        hh_size = "hh_size",
        weight  = "weight"
      )
    } else {
      x$wash_sharing_sanitation_facility_n_ind <- "19_and_below"
    }

    # --- 2f. Handwashing Facility JMP Cat ---
    #  Mozambique survey is in-person with observed water and soap type columns.
    #  We map the observed columns to what humind expects.
    if (all(c("wash_handwashing_facility_observed_water",
              "wash_handwashing_facility_observed_soap") %in% names(x))) {

      # Create the survey_modality column (all in-person)
      x$survey_modality <- "in_person"

      # Map the observed soap column to soap_type (humind expects wash_soap_observed_type)
      x$wash_soap_observed_type <- as.character(x$wash_handwashing_facility_observed_soap)

      # Create the handwashing facility availability column
      # If water was observed at all, a facility exists
      x$wash_handwashing_facility <- dplyr::case_when(
        is.na(x$wash_handwashing_facility_observed_water) ~ "none",
        TRUE ~ "available_fixed_in_dwelling"
      )

      # Rename observed water column to expected name
      x$wash_handwashing_facility_observed_water_yn <- as.character(
        x$wash_handwashing_facility_observed_water
      )

      # Map soap observed column to soap_observed_yn (availability yes/no)
      x$wash_soap_observed_yn <- dplyr::case_when(
        x$wash_soap_observed_type %in% c("soap", "detergent", "ash_mud_sand") ~ "soap_available",
        x$wash_soap_observed_type %in% c("not_available") ~ "soap_not_available",
        TRUE ~ "soap_not_available"
      )

      # Create placeholder columns for remote-survey variants that
      # add_handwashing_facility_cat always checks for existence
      if (!"wash_handwashing_facility_reported" %in% names(x))
        x$wash_handwashing_facility_reported <- NA_character_
      if (!"wash_handwashing_facility_water_reported_yn" %in% names(x))
        x$wash_handwashing_facility_water_reported_yn <- NA_character_
      if (!"wash_soap_reported_yn" %in% names(x))
        x$wash_soap_reported_yn <- NA_character_
      if (!"wash_soap_reported_type" %in% names(x))
        x$wash_soap_reported_type <- NA_character_

      x <- humind::add_handwashing_facility_cat(
        x,
        survey_modality            = "survey_modality",
        survey_modality_in_person  = "in_person",
        facility                   = "wash_handwashing_facility",
        facility_yes               = c("available_fixed_in_dwelling", "available_fixed_in_plot", "available_mobile"),
        facility_no                = "none",
        facility_observed_water    = "wash_handwashing_facility_observed_water_yn",
        facility_observed_water_yes = "water_available",
        facility_observed_water_no  = "water_not_available",
        facility_observed_soap     = "wash_soap_observed_yn",
        facility_observed_soap_yes = "soap_available",
        facility_observed_soap_no  = "soap_not_available",
        soap_type_observed         = "wash_soap_observed_type",
        soap_type_observed_yes     = c("soap", "detergent"),
        soap_type_observed_no      = c("ash_mud_sand"),
        soap_type_observed_undefined = c("dnk", "pnta", "other", "not_available")
      )
    } else {
      x$wash_handwashing_facility_jmp_cat <- "undefined"
    }

    # --- 2g. Water Quantity Score (HWISE-4) ---
    #  No HWISE questions in Mozambique MSNA 2026; use a proxy from
    #  wash_drinking_water_quantity if available.
    if ("wash_drinking_water_quantity" %in% names(x)) {
      wq <- as.character(x$wash_drinking_water_quantity)
      x$comp_wash_score_water_quantity <- dplyr::case_when(
        wq %in% c("never_available", "rarely_available") ~ 4L,
        wq == "sometimes_available"   ~ 3L,
        wq == "often_available"       ~ 2L,
        wq == "always_available"      ~ 1L,
        TRUE                          ~ 1L
      )
    } else {
      x$comp_wash_score_water_quantity <- 1L
    }

    # --- 2h. WASH Composite via humind::add_comp_wash() ---
    #  Map the setting values to what humind expects
    x$setting_original <- x$setting
    x$setting <- dplyr::case_when(
      x$setting %in% c("camp_formal", "camp_informal") ~ x$setting,
      x$setting %in% c("urban", "peri_urban") ~ "urban",
      x$setting == "rural"  ~ "rural",
      TRUE                  ~ "rural"
    )

    x <- humind::add_comp_wash(x)

    # Restore original setting
    x$setting <- x$setting_original
    x$setting_original <- NULL

    x$comp_wash_in_need <- as.numeric(x$comp_wash_score >= 3)
    x$comp_wash_in_severe_need <- as.numeric(x$comp_wash_score >= 4)
  } else {
    x$comp_wash_score <- 1
    x$comp_wash_in_need <- 0
    x$comp_wash_in_severe_need <- 0
  }

  # 3. Health (Health LSG) ---------------------------------------------------
  message("    -> Health Composite (comp_health_score)...")

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

  # 4. Shelter & Non-Food Items (SNFI LSG) -----------------------------------
  message("    -> Shelter & NFI Composite (comp_snfi_score)...")

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

  # 5. Protection (Protection LSG) -------------------------------------------
  message("    -> Protection Composite (comp_prot_score)...")

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

  # 6. Ensure Education Composite exists -------------------------------------
  if (!"comp_edu_score" %in% names(x)) {
    x$comp_edu_score <- 1
    x$comp_edu_in_need <- 0
    x$comp_edu_in_severe_need <- 0
  }

  # 7. Global Multi-Sector Needs Index (MSNI) Aggregation -------------------
  message("    -> Global MSNI Aggregation (humind::add_msni)...")

  sector_cols <- c("comp_foodsec_score", "comp_snfi_score", "comp_wash_score",
                   "comp_prot_score", "comp_health_score", "comp_edu_score")

  for (col in sector_cols) {
    x[[col]] <- to_numeric(x[[col]])
    x[[col]][is.na(x[[col]])] <- 1
  }

  x <- humind::add_msni(x)

  # Multi-sectoral co-occurrence (number of sectors with severity >= 3)
  in_need_cols <- c("comp_foodsec_in_need", "comp_snfi_in_need", "comp_wash_in_need",
                    "comp_prot_in_need", "comp_health_in_need", "comp_edu_in_need")

  for (col in in_need_cols) {
    if (col %in% names(x)) x[[col]] <- to_numeric(x[[col]])
  }

  x$msni_sectors_in_need_n <- rowSums(x[, intersect(in_need_cols, names(x))], na.rm = TRUE)

  message("--> [Humind MSNI] Calculation completed successfully!")
  return(x)
}
