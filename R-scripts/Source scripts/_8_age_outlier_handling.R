# ============================================================================
# AGE OUTLIER DETECTION & CORRECTION
# Lines to insert at 8.Generate the clean df and review the cleaning.R:1018
# ============================================================================
# This section identifies age values exceeding 100 years (impossible values)
# and blanks them out while recording the corrections in cleaning logs.
# ============================================================================

cat(magenta(paste(rep("=", 80), collapse = "")), "\n")
cat(magenta("AGE OUTLIER DETECTION & CORRECTION"), "\n")
cat(magenta(paste(rep("=", 80), collapse = "")), "\n\n")

# ============================================================================
# STEP 1: IDENTIFY AGE OUTLIERS (> 100 years)
# ============================================================================

age_outlier_corrections <- list()

# 1A. ROSTER DATASET
cat(cyan("Checking ROSTER for age > 100..."), "\n")
roster_age_problems <- clean_roster %>%
  filter(ind_age > 100) %>%
  mutate(
    dataset = "roster",
    change_type = "blank_response",
    issue = "Age value exceeds 100 years - data entry error or impossible value",
    check_id = "age_outlier_check_roster",
    check_to_perform = "Identify age values > 100",
    columns_to_clean = "ind_age",
    notes = sprintf("Original value: %d (impossible age)", ind_age),
    question = "ind_age",
    old_value = as.character(ind_age),
    new_value = NA_character_,
    uuid = uuid,
    parent_uuid = parent_uuid,
    repeat_index = as.character(repeat_index)
  ) %>%
  select(dataset, uuid, parent_uuid, repeat_index, question, old_value, new_value, 
         change_type, issue, check_id, check_to_perform, columns_to_clean, notes)

if (nrow(roster_age_problems) > 0) {
  age_outlier_corrections[["roster"]] <- roster_age_problems
  cat(red(sprintf("✗ Found %d records with ind_age > 100\n", nrow(roster_age_problems))))
}

# 1B. HEALTH INDICATOR
cat(cyan("Checking HEALTH INDICATOR for age > 100..."), "\n")
health_age_problems <- clean_health_ind %>%
  filter(health_ind_age > 100) %>%
  mutate(
    dataset = "health_ind",
    change_type = "blank_response",
    issue = "Age value exceeds 100 years - data entry error or impossible value",
    check_id = "age_outlier_check_health",
    check_to_perform = "Identify age values > 100",
    columns_to_clean = "health_ind_age",
    notes = sprintf("Original value: %d (impossible age)", health_ind_age),
    question = "health_ind_age",
    old_value = as.character(health_ind_age),
    new_value = NA_character_,
    uuid = uuid,
    parent_uuid = parent_uuid,
    repeat_index = as.character(repeat_index)
  ) %>%
  select(dataset, uuid, parent_uuid, repeat_index, question, old_value, new_value,
         change_type, issue, check_id, check_to_perform, columns_to_clean, notes)

if (nrow(health_age_problems) > 0) {
  age_outlier_corrections[["health"]] <- health_age_problems
  cat(red(sprintf("✗ Found %d records with health_ind_age > 100\n", nrow(health_age_problems))))
}

# 1C. EDUCATION INDICATOR
cat(cyan("Checking EDUCATION INDICATOR for age > 100..."), "\n")
edu_age_problems <- clean_edu_ind %>%
  filter(edu_ind_age > 100) %>%
  mutate(
    dataset = "edu_ind",
    change_type = "blank_response",
    issue = "Age value exceeds 100 years - data entry error or impossible value",
    check_id = "age_outlier_check_edu",
    check_to_perform = "Identify age values > 100",
    columns_to_clean = "edu_ind_age",
    notes = sprintf("Original value: %d (impossible age)", edu_ind_age),
    question = "edu_ind_age",
    old_value = as.character(edu_ind_age),
    new_value = NA_character_,
    uuid = uuid,
    parent_uuid = parent_uuid,
    repeat_index = as.character(repeat_index)
  ) %>%
  select(dataset, uuid, parent_uuid, repeat_index, question, old_value, new_value,
         change_type, issue, check_id, check_to_perform, columns_to_clean, notes)

if (nrow(edu_age_problems) > 0) {
  age_outlier_corrections[["edu"]] <- edu_age_problems
  cat(red(sprintf("✗ Found %d records with edu_ind_age > 100\n", nrow(edu_age_problems))))
}

# 1D. NUTRITION INDICATOR - ind_age
cat(cyan("Checking NUTRITION INDICATOR for age > 100..."), "\n")
nut_age_problems <- clean_nutrition_ind %>%
  filter(nut_ind_age > 100) %>%
  mutate(
    dataset = "nutrition_ind",
    change_type = "blank_response",
    issue = "Age value exceeds 100 years - data entry error or impossible value",
    check_id = "age_outlier_check_nutrition",
    check_to_perform = "Identify age values > 100",
    columns_to_clean = "nut_ind_age",
    notes = sprintf("Original value: %d (impossible age)", nut_ind_age),
    question = "nut_ind_age",
    old_value = as.character(nut_ind_age),
    new_value = NA_character_,
    uuid = uuid,
    parent_uuid = parent_uuid,
    repeat_index = as.character(repeat_index)
  ) %>%
  select(dataset, uuid, parent_uuid, repeat_index, question, old_value, new_value,
         change_type, issue, check_id, check_to_perform, columns_to_clean, notes)

if (nrow(nut_age_problems) > 0) {
  age_outlier_corrections[["nut_age"]] <- nut_age_problems
  cat(red(sprintf("✗ Found %d records with nut_ind_age > 100\n", nrow(nut_age_problems))))
}

# 1E. NUTRITION INDICATOR - under5_age_months
cat(cyan("Checking NUTRITION INDICATOR for months > 100..."), "\n")
nut_months_problems <- clean_nutrition_ind %>%
  filter(nut_ind_under5_age_months > 100) %>%
  mutate(
    dataset = "nutrition_ind",
    change_type = "blank_response",
    issue = "Age in months > 100 (>8.3 years) - exceeds under-5 category by definition",
    check_id = "age_outlier_check_nut_months",
    check_to_perform = "Identify age values in months > 100",
    columns_to_clean = "nut_ind_under5_age_months",
    notes = sprintf("Original value: %d months (~%.1f years) - incompatible with under-5 classification", 
                    nut_ind_under5_age_months, nut_ind_under5_age_months/12),
    question = "nut_ind_under5_age_months",
    old_value = as.character(nut_ind_under5_age_months),
    new_value = NA_character_,
    uuid = uuid,
    parent_uuid = parent_uuid,
    repeat_index = as.character(repeat_index)
  ) %>%
  select(dataset, uuid, parent_uuid, repeat_index, question, old_value, new_value,
         change_type, issue, check_id, check_to_perform, columns_to_clean, notes)

if (nrow(nut_months_problems) > 0) {
  age_outlier_corrections[["nut_months"]] <- nut_months_problems
  cat(red(sprintf("✗ Found %d records with nut_ind_under5_age_months > 100\n", nrow(nut_months_problems))))
}

# 1F. PROTECTION INDICATOR
cat(cyan("Checking PROTECTION INDICATOR for age > 100..."), "\n")
prot_age_problems <- clean_child_protection_ind %>%
  filter(prot_ind_age > 100) %>%
  mutate(
    dataset = "child_protection_ind",
    change_type = "blank_response",
    issue = "Age value exceeds 100 years - data entry error or impossible value",
    check_id = "age_outlier_check_protection",
    check_to_perform = "Identify age values > 100",
    columns_to_clean = "prot_ind_age",
    notes = sprintf("Original value: %d (impossible age)", prot_ind_age),
    question = "prot_ind_age",
    old_value = as.character(prot_ind_age),
    new_value = NA_character_,
    uuid = uuid,
    parent_uuid = parent_uuid,
    repeat_index = as.character(repeat_index)
  ) %>%
  select(dataset, uuid, parent_uuid, repeat_index, question, old_value, new_value,
         change_type, issue, check_id, check_to_perform, columns_to_clean, notes)

if (nrow(prot_age_problems) > 0) {
  age_outlier_corrections[["prot"]] <- prot_age_problems
  cat(red(sprintf("✗ Found %d records with prot_ind_age > 100\n", nrow(prot_age_problems))))
}

# Summary
total_outliers <- sum(sapply(age_outlier_corrections, nrow))
cat("\n")
if (total_outliers > 0) {
  cat(red(sprintf("TOTAL AGE OUTLIERS FOUND: %d records\n", total_outliers)))
} else {
  cat(green("No age outliers found\n"))
}

# ============================================================================
# STEP 2: APPEND CORRECTIONS TO CLEANING LOGS
# ============================================================================

if (total_outliers > 0) {
  cat("\n", cyan("APPLYING CORRECTIONS TO CLEANING LOGS"), "\n")
  cat(paste(rep("─", 80), collapse = ""), "\n\n")
  
  correction_count <- 0
  
  # 2A. ROSTER CLEANING LOG
  if ("roster" %in% names(age_outlier_corrections)) {
    new_entries <- age_outlier_corrections$roster %>%
      mutate(
        repeat_index = as.character(repeat_index),
        uuid = as.character(uuid),
        parent_uuid = as.character(parent_uuid)
      )
    
    cleaning_log_roster <- cleaning_log_roster %>%
      mutate(repeat_index = as.character(repeat_index)) %>%
      bind_rows(new_entries)
    
    cat(green("✓"), sprintf("Added %d entries to cleaning_log_roster\n", nrow(new_entries)))
    correction_count <- correction_count + nrow(new_entries)
  }
  
  # 2B. HEALTH CLEANING LOG  
  if ("health" %in% names(age_outlier_corrections)) {
    new_entries <- age_outlier_corrections$health %>%
      mutate(
        repeat_index = as.character(repeat_index),
        uuid = as.character(uuid),
        parent_uuid = as.character(parent_uuid)
      )
    
    cleaning_log_health_ind <- cleaning_log_health_ind %>%
      mutate(repeat_index = as.character(repeat_index)) %>%
      bind_rows(new_entries)
    
    cat(green("✓"), sprintf("Added %d entries to cleaning_log_health_ind\n", nrow(new_entries)))
    correction_count <- correction_count + nrow(new_entries)
  }
  
  # 2C. EDUCATION CLEANING LOG
  if ("edu" %in% names(age_outlier_corrections)) {
    new_entries <- age_outlier_corrections$edu %>%
      mutate(
        repeat_index = as.character(repeat_index),
        uuid = as.character(uuid),
        parent_uuid = as.character(parent_uuid)
      )
    
    cleaning_log_edu_ind <- cleaning_log_edu_ind %>%
      mutate(repeat_index = as.character(repeat_index)) %>%
      bind_rows(new_entries)
    
    cat(green("✓"), sprintf("Added %d entries to cleaning_log_edu_ind\n", nrow(new_entries)))
    correction_count <- correction_count + nrow(new_entries)
  }
  
  # 2D. NUTRITION CLEANING LOG
  nut_corrections_list <- list()
  if ("nut_age" %in% names(age_outlier_corrections)) {
    nut_corrections_list[[1]] <- age_outlier_corrections$nut_age
  }
  if ("nut_months" %in% names(age_outlier_corrections)) {
    nut_corrections_list[[length(nut_corrections_list) + 1]] <- age_outlier_corrections$nut_months
  }
  
  if (length(nut_corrections_list) > 0) {
    nut_corrections <- bind_rows(nut_corrections_list) %>%
      mutate(
        repeat_index = as.character(repeat_index),
        uuid = as.character(uuid),
        parent_uuid = as.character(parent_uuid)
      )
    
    cleaning_log_nutrition <- cleaning_log_nutrition %>%
      mutate(repeat_index = as.character(repeat_index)) %>%
      bind_rows(nut_corrections)
    
    cat(green("✓"), sprintf("Added %d entries to cleaning_log_nutrition\n", nrow(nut_corrections)))
    correction_count <- correction_count + nrow(nut_corrections)
  }
  
  # 2E. PROTECTION CLEANING LOG
  if ("prot" %in% names(age_outlier_corrections)) {
    # Create cleaning log for protection if it doesn't exist
    if (!exists("cleaning_log_child_protection_ind")) {
      cleaning_log_child_protection_ind <- empty_cleaning_log()
    }
    
    new_entries <- age_outlier_corrections$prot %>%
      mutate(
        repeat_index = as.character(repeat_index),
        uuid = as.character(uuid),
        parent_uuid = as.character(parent_uuid)
      )
    
    cleaning_log_child_protection_ind <- cleaning_log_child_protection_ind %>%
      mutate(repeat_index = as.character(repeat_index)) %>%
      bind_rows(new_entries)
    
    cat(green("✓"), sprintf("Added %d entries to cleaning_log_child_protection_ind\n", nrow(new_entries)))
    correction_count <- correction_count + nrow(new_entries)
  }
  
  cat("\n")
  cat(paste(rep("─", 80), collapse = ""), "\n")
  cat(green(sprintf("✅ TOTAL: Added %d age outlier corrections to cleaning logs\n", correction_count)))
  cat(paste(rep("─", 80), collapse = ""), "\n\n")
  
  # Export age outliers report
  age_outliers_export <- bind_rows(age_outlier_corrections)
  
  write_csv(
    age_outliers_export,
    here::here("outputs", "age_outliers_detected_and_corrected.csv")
  )
  
  cat(green("✓ Age outliers report exported: age_outliers_detected_and_corrected.csv\n"))
} else {
  cat(green("\n✓ No age outliers to correct\n"))
}

cat("\n")
