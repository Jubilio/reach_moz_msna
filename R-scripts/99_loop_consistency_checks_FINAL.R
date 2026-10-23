# ============================================================================
# COMPREHENSIVE LOOP CONSISTENCY CHECKS - FINAL VERSION
# ============================================================================
# Purpose: Identify and classify discrepancies between household and repeat datasets
# ============================================================================

library(tidyverse)
library(here)

cat("\n", paste(rep("=", 100), collapse = ""), "\n")
cat("LOOP CONSISTENCY QUALITY CONTROL CHECKS\n")
cat(paste(rep("=", 100), collapse = ""), "\n\n")

# ============================================================================
# 1. HOUSEHOLD SIZE vs ROSTER: DETAILED CONSISTENCY CHECK
# ============================================================================

cat(paste(rep("-", 100), collapse = ""), "\n")
cat("CHECK 1: HOUSEHOLD SIZE vs ROSTER CONSISTENCY\n")
cat(paste(rep("-", 100), collapse = ""), "\n\n")

# Calculate roster counts per household
roster_counts <- clean_roster |>
  group_by(parent_uuid) |>
  summarise(
    roster_count = n(),
    person_ids_in_roster = list(person_id),
    .groups = "drop"
  )

# Get household-level data with cleaning context
household_checks <- clean_main |>
  select(
    household_uuid = `_uuid`,
    hh_size,
    point_number,
    admin1,
    admin2,
    admin3,
    admin3_setlement,
    enum_id,
    username
  ) |>
  left_join(
    roster_counts,
    by = c("household_uuid" = "parent_uuid")
  ) |>
  mutate(
    roster_count = coalesce(roster_count, 0L),
    roster_difference = roster_count - hh_size,
    roster_consistent = roster_count == hh_size
  )

# Identify inconsistencies
roster_inconsistencies <- household_checks |>
  filter(!roster_consistent) |>
  mutate(
    issue_type = "HH_SIZE_ROSTER_COUNT_MISMATCH",
    difference_type = case_when(
      roster_difference > 0 ~ paste0("Extra: +", roster_difference),
      roster_difference < 0 ~ paste0("Missing: ", roster_difference)
    )
  ) |>
  arrange(desc(abs(roster_difference)))

# Summary statistics
cat("Total households checked:", nrow(household_checks), "\n")
cat("Households with roster-size mismatch:", nrow(roster_inconsistencies), "\n")
cat("Total roster records:", sum(household_checks$roster_count), "\n")
cat("Total hh_size reported:", sum(clean_main$hh_size), "\n")
cat("Net difference (roster - hh_size):", sum(household_checks$roster_difference), "\n\n")

# Breakdown
cat("Breakdown of mismatches:\n")
print(roster_inconsistencies |> count(difference_type))
cat("\n")

# ============================================================================
# 2. EDUCATION LOOP CONSISTENCY
# ============================================================================

cat("\n", paste(rep("-", 100), collapse = ""), "\n")
cat("CHECK 2: EDUCATION LOOP CONSISTENCY\n")
cat(paste(rep("-", 100), collapse = ""), "\n\n")

# Determine eligibility: use ind_age_schooling if available
if ("ind_age_schooling" %in% names(clean_roster)) {
  cat("Using ind_age_schooling to identify education-eligible members\n\n")
  
  edu_eligible_roster <- clean_roster |>
    filter(ind_age_schooling == 1) |>
    group_by(parent_uuid) |>
    summarise(
      expected_edu_count = n(),
      expected_edu_persons = list(person_id),
      .groups = "drop"
    )
} else {
  cat("Using all roster members as education-eligible baseline\n\n")
  edu_eligible_roster <- clean_roster |>
    group_by(parent_uuid) |>
    summarise(
      expected_edu_count = n(),
      expected_edu_persons = list(person_id),
      .groups = "drop"
    )
}

# Observed education records
edu_observed <- clean_edu_ind |>
  group_by(parent_uuid) |>
  summarise(
    observed_edu_count = n(),
    observed_edu_persons = list(edu_person_id),
    .groups = "drop"
  )

# Compare
edu_comparison <- household_checks |>
  select(household_uuid, hh_size, point_number, enum_id, admin1, admin2, admin3, admin3_setlement, username) |>
  left_join(edu_eligible_roster, by = c("household_uuid" = "parent_uuid")) |>
  left_join(edu_observed, by = c("household_uuid" = "parent_uuid")) |>
  mutate(
    expected_edu_count = coalesce(expected_edu_count, 0L),
    observed_edu_count = coalesce(observed_edu_count, 0L),
    edu_difference = observed_edu_count - expected_edu_count,
    edu_consistent = observed_edu_count == expected_edu_count
  )

# Find inconsistencies
edu_inconsistencies <- edu_comparison |>
  filter(!edu_consistent)

cat("Education loop summary:\n")
cat("  Households checked:", nrow(edu_comparison), "\n")
cat("  Households with education records:", sum(edu_comparison$observed_edu_count > 0), "\n")
cat("  Expected education-eligible members:", sum(edu_comparison$expected_edu_count), "\n")
cat("  Observed education records:", sum(edu_comparison$observed_edu_count), "\n")
cat("  Households with inconsistency:", nrow(edu_inconsistencies), "\n\n")

# ============================================================================
# 3. HEALTH LOOP CONSISTENCY
# ============================================================================

cat("\n", paste(rep("-", 100), collapse = ""), "\n")
cat("CHECK 3: HEALTH LOOP CONSISTENCY\n")
cat(paste(rep("-", 100), collapse = ""), "\n\n")

# Health: all roster members are eligible
health_expected_roster <- clean_roster |>
  group_by(parent_uuid) |>
  summarise(
    expected_health_count = n(),
    expected_health_persons = list(person_id),
    .groups = "drop"
  )

# Observed health records
health_observed <- clean_health_ind |>
  group_by(parent_uuid) |>
  summarise(
    observed_health_count = n(),
    observed_health_persons = list(health_person_id),
    .groups = "drop"
  )

# Compare
health_comparison <- household_checks |>
  select(household_uuid, hh_size, point_number, enum_id, admin1, admin2, admin3, admin3_setlement, username) |>
  left_join(health_expected_roster, by = c("household_uuid" = "parent_uuid")) |>
  left_join(health_observed, by = c("household_uuid" = "parent_uuid")) |>
  mutate(
    expected_health_count = coalesce(expected_health_count, 0L),
    observed_health_count = coalesce(observed_health_count, 0L),
    health_difference = observed_health_count - expected_health_count,
    health_consistent = observed_health_count == expected_health_count
  )

health_inconsistencies <- health_comparison |>
  filter(!health_consistent)

cat("Health loop summary:\n")
cat("  Households checked:", nrow(health_comparison), "\n")
cat("  Households with health records:", sum(health_comparison$observed_health_count > 0), "\n")
cat("  Expected health records (all roster):", sum(health_comparison$expected_health_count), "\n")
cat("  Observed health records:", sum(health_comparison$observed_health_count), "\n")
cat("  Households with inconsistency:", nrow(health_inconsistencies), "\n\n")

# ============================================================================
# 4. NUTRITION LOOP CONSISTENCY
# ============================================================================

cat("\n", paste(rep("-", 100), collapse = ""), "\n")
cat("CHECK 4: NUTRITION LOOP CONSISTENCY\n")
cat(paste(rep("-", 100), collapse = ""), "\n\n")

# Nutrition: children under 5 and women
if ("ind_age_0_5" %in% names(clean_roster) && "ind_woman_repr_age" %in% names(clean_roster)) {
  cat("Using ind_age_0_5 and ind_woman_repr_age for eligibility\n\n")
  
  nut_eligible_roster <- clean_roster |>
    filter(ind_age_0_5 == 1 | ind_woman_repr_age == 1) |>
    group_by(parent_uuid) |>
    summarise(
      expected_nut_count = n(),
      expected_nut_persons = list(person_id),
      .groups = "drop"
    )
} else {
  cat("Eligibility variables not found; using all roster\n\n")
  nut_eligible_roster <- clean_roster |>
    group_by(parent_uuid) |>
    summarise(
      expected_nut_count = n(),
      expected_nut_persons = list(person_id),
      .groups = "drop"
    )
}

# Observed nutrition records
nut_observed <- clean_nutrition_ind |>
  group_by(parent_uuid) |>
  summarise(
    observed_nut_count = n(),
    observed_nut_persons = list(nut_person_id),
    .groups = "drop"
  )

# Compare
nut_comparison <- household_checks |>
  select(household_uuid, hh_size, point_number, enum_id, admin1, admin2, admin3, admin3_setlement, username) |>
  left_join(nut_eligible_roster, by = c("household_uuid" = "parent_uuid")) |>
  left_join(nut_observed, by = c("household_uuid" = "parent_uuid")) |>
  mutate(
    expected_nut_count = coalesce(expected_nut_count, 0L),
    observed_nut_count = coalesce(observed_nut_count, 0L),
    nut_difference = observed_nut_count - expected_nut_count,
    nut_consistent = observed_nut_count == expected_nut_count
  )

nut_inconsistencies <- nut_comparison |>
  filter(!nut_consistent)

cat("Nutrition loop summary:\n")
cat("  Households checked:", nrow(nut_comparison), "\n")
cat("  Households with nutrition records:", sum(nut_comparison$observed_nut_count > 0), "\n")
cat("  Expected nutrition-eligible members:", sum(nut_comparison$expected_nut_count), "\n")
cat("  Observed nutrition records:", sum(nut_comparison$observed_nut_count), "\n")
cat("  Households with inconsistency:", nrow(nut_inconsistencies), "\n\n")

# ============================================================================
# 5. CHILD PROTECTION LOOP CONSISTENCY
# ============================================================================

cat("\n", paste(rep("-", 100), collapse = ""), "\n")
cat("CHECK 5: CHILD PROTECTION LOOP CONSISTENCY\n")
cat(paste(rep("-", 100), collapse = ""), "\n\n")

# Child protection: children/youth under 18
if ("ind_age_below18" %in% names(clean_roster)) {
  cat("Using ind_age_below18 for eligibility\n\n")
  
  prot_eligible_roster <- clean_roster |>
    filter(ind_age_below18 == 1) |>
    group_by(parent_uuid) |>
    summarise(
      expected_prot_count = n(),
      expected_prot_persons = list(person_id),
      .groups = "drop"
    )
} else {
  cat("Eligibility variable not found; using all roster\n\n")
  prot_eligible_roster <- clean_roster |>
    group_by(parent_uuid) |>
    summarise(
      expected_prot_count = n(),
      expected_prot_persons = list(person_id),
      .groups = "drop"
    )
}

# Observed protection records
prot_observed <- clean_child_protection_ind |>
  group_by(parent_uuid) |>
  summarise(
    observed_prot_count = n(),
    observed_prot_persons = list(prot_person_id),
    .groups = "drop"
  )

# Compare
prot_comparison <- household_checks |>
  select(household_uuid, hh_size, point_number, enum_id, admin1, admin2, admin3, admin3_setlement, username) |>
  left_join(prot_eligible_roster, by = c("household_uuid" = "parent_uuid")) |>
  left_join(prot_observed, by = c("household_uuid" = "parent_uuid")) |>
  mutate(
    expected_prot_count = coalesce(expected_prot_count, 0L),
    observed_prot_count = coalesce(observed_prot_count, 0L),
    prot_difference = observed_prot_count - expected_prot_count,
    prot_consistent = observed_prot_count == expected_prot_count
  )

prot_inconsistencies <- prot_comparison |>
  filter(!prot_consistent)

cat("Child protection loop summary:\n")
cat("  Households checked:", nrow(prot_comparison), "\n")
cat("  Households with protection records:", sum(prot_comparison$observed_prot_count > 0), "\n")
cat("  Expected child protection-eligible:", sum(prot_comparison$expected_prot_count), "\n")
cat("  Observed protection records:", sum(prot_comparison$observed_prot_count), "\n")
cat("  Households with inconsistency:", nrow(prot_inconsistencies), "\n\n")

# ============================================================================
# 6. CREATE SUMMARY TABLE
# ============================================================================

cat("\n", paste(rep("-", 100), collapse = ""), "\n")
cat("SUMMARY TABLE: LOOP CONSISTENCY ACROSS ALL DATASETS\n")
cat(paste(rep("-", 100), collapse = ""), "\n\n")

loop_consistency_summary <- tibble(
  loop = c("roster", "education", "health", "nutrition", "child_protection"),
  households_checked = c(
    nrow(household_checks),
    nrow(edu_comparison),
    nrow(health_comparison),
    nrow(nut_comparison),
    nrow(prot_comparison)
  ),
  households_with_issues = c(
    nrow(roster_inconsistencies),
    nrow(edu_inconsistencies),
    nrow(health_inconsistencies),
    nrow(nut_inconsistencies),
    nrow(prot_inconsistencies)
  ),
  pct_with_issues = c(
    round(nrow(roster_inconsistencies) / nrow(household_checks) * 100, 1),
    round(nrow(edu_inconsistencies) / nrow(edu_comparison) * 100, 1),
    round(nrow(health_inconsistencies) / nrow(health_comparison) * 100, 1),
    round(nrow(nut_inconsistencies) / nrow(nut_comparison) * 100, 1),
    round(nrow(prot_inconsistencies) / nrow(prot_comparison) * 100, 1)
  ),
  expected_records = c(
    sum(household_checks$hh_size),
    sum(edu_comparison$expected_edu_count),
    sum(health_comparison$expected_health_count),
    sum(nut_comparison$expected_nut_count),
    sum(prot_comparison$expected_prot_count)
  ),
  observed_records = c(
    sum(household_checks$roster_count),
    sum(edu_comparison$observed_edu_count),
    sum(health_comparison$observed_health_count),
    sum(nut_comparison$observed_nut_count),
    sum(prot_comparison$observed_prot_count)
  ),
  record_difference = c(
    sum(household_checks$roster_difference),
    sum(edu_comparison$edu_difference),
    sum(health_comparison$health_difference),
    sum(nut_comparison$nut_difference),
    sum(prot_comparison$prot_difference)
  )
)

print(loop_consistency_summary)

# ============================================================================
# 7. EXPORT RESULTS
# ============================================================================

cat("\n\n", paste(rep("-", 100), collapse = ""), "\n")
cat("EXPORTING RESULTS\n")
cat(paste(rep("-", 100), collapse = ""), "\n\n")

# Export roster inconsistencies
write_csv(
  roster_inconsistencies |> select(-person_ids_in_roster),
  here("outputs", "qc_roster_size_inconsistencies.csv")
)
cat("✓ Exported: qc_roster_size_inconsistencies.csv (", nrow(roster_inconsistencies), " rows )\n")

# Export summary table
write_csv(
  loop_consistency_summary,
  here("outputs", "qc_loop_consistency_summary.csv")
)
cat("✓ Exported: qc_loop_consistency_summary.csv\n")

# Export education inconsistencies
write_csv(
  edu_inconsistencies |> select(-expected_edu_persons, -observed_edu_persons),
  here("outputs", "qc_education_loop_inconsistencies.csv")
)
cat("✓ Exported: qc_education_loop_inconsistencies.csv (", nrow(edu_inconsistencies), " rows )\n")

# Export health inconsistencies
write_csv(
  health_inconsistencies |> select(-expected_health_persons, -observed_health_persons),
  here("outputs", "qc_health_loop_inconsistencies.csv")
)
cat("✓ Exported: qc_health_loop_inconsistencies.csv (", nrow(health_inconsistencies), " rows )\n")

# Export nutrition inconsistencies
write_csv(
  nut_inconsistencies |> select(-expected_nut_persons, -observed_nut_persons),
  here("outputs", "qc_nutrition_loop_inconsistencies.csv")
)
cat("✓ Exported: qc_nutrition_loop_inconsistencies.csv (", nrow(nut_inconsistencies), " rows )\n")

# Export protection inconsistencies
write_csv(
  prot_inconsistencies |> select(-expected_prot_persons, -observed_prot_persons),
  here("outputs", "qc_protection_loop_inconsistencies.csv")
)
cat("✓ Exported: qc_protection_loop_inconsistencies.csv (", nrow(prot_inconsistencies), " rows )\n")

# ============================================================================
# 8. FINAL SUMMARY
# ============================================================================

cat("\n\n", paste(rep("=", 100), collapse = ""), "\n")
cat("FINAL STATUS\n")
cat(paste(rep("=", 100), collapse = ""), "\n\n")

cat("✓ Loop consistency checks completed\n")
cat("✓ All objects created and available in R environment:\n")
cat("    - household_checks (all households with consistency flags)\n")
cat("    - roster_inconsistencies\n")
cat("    - edu_comparison, edu_inconsistencies\n")
cat("    - health_comparison, health_inconsistencies\n")
cat("    - nut_comparison, nut_inconsistencies\n")
cat("    - prot_comparison, prot_inconsistencies\n")
cat("    - loop_consistency_summary\n")
cat("✓ CSV files exported to outputs/\n")
cat("✓ No data modifications made (diagnostic only)\n")
cat("✓ Ready for quality assurance review\n\n")
