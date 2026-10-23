# Consistency Harmonization & Age Indicator Correction
## Code Documentation (Lines 1585+)

### Overview
This section addresses two critical data quality issues identified in the cleaning process:
1. **Loop sheet inconsistencies** - individuals removed from roster still exist in education, health, nutrition, and child protection loops
2. **Age demographic indicator mismatches** - indicators no longer reconcile after roster and household size updates

---

## Section 1: Loop Sheet Consistency Check
**Lines: 1585-1707**

### Purpose
Ensures that any person_id removed from the roster is also removed from all individual-level loop sheets to maintain referential integrity.

### Key Components

#### A. Create Reference Roster (Lines 1590-1596)
```r
valid_roster <- clean_roster |>
  select(parent_uuid, person_id) |>
  distinct() |>
  arrange(parent_uuid, person_id)
```
- Extracts all valid (parent_uuid, person_id) combinations from the cleaned roster
- This becomes the "source of truth" for valid individuals in each household

#### B. Loop Dataset Definition (Lines 1599-1604)
Defines all loop sheets to be checked:
- Education loop (clean_edu_ind)
- Health loop (clean_health_ind)
- Nutrition loop (clean_nutrition_ind)
- Child Protection loop (clean_child_protection_ind)

#### C. Consistency Checking Loop (Lines 1610-1653)
For each loop sheet:
- Uses `anti_join()` to find records in loop that DON'T exist in valid_roster
- Identifies orphaned person_ids (e.g., individuals removed from roster but still in education loop)
- Stores problematic records for removal and logging
- Provides console output showing affected records

#### D. Record Removal (Lines 1654-1681)
Removes identified invalid records from each loop sheet:
```r
clean_edu_ind <- clean_edu_ind |>
  anti_join(edu_removals, by = c("parent_uuid", "person_id"))
```

### Example Use Case
**Scenario:** UUID 38369a70-e3a5-4807-8618-34d03ed0a356
- After roster cleanup: 6 valid individuals
- Education loop had 7 records (person_id: 7 remained but shouldn't)
- **Action:** Remove person_id 7 record from education loop
- **Result:** Education loop now has 6 records matching roster

---

## Section 2: Age Demographic Indicator Recalculation
**Lines: 1710-1798**

### Purpose
After updating roster (removing individuals), recalculate age-based household indicators:
- `ind_age_below18_n` - count of individuals under 18
- `ind_age_above18_n` - count of individuals 18 and above

### Key Components

#### A. Age Recalculation Logic (Lines 1714-1732)
```r
age_recalculation <- clean_roster |>
  group_by(parent_uuid) |>
  summarise(
    new_ind_age_below18_n = sum(age < 18, na.rm = TRUE),
    new_ind_age_above18_n = sum(age >= 18, na.rm = TRUE),
    roster_size = n(),
    .groups = "drop"
  )
```
- Recalculates age group counts directly from updated roster
- Creates a validation check: age_sum should equal roster_size AND hh_size

#### B. Identify Corrections Needed (Lines 1734-1747)
Compares old vs. new values to find households where indicators changed:
- `below18_changed` - old count ≠ new count for under-18
- `above18_changed` - old count ≠ new count for 18+

#### C. Apply Corrections to Main Dataset (Lines 1759-1776)
```r
clean_main <- clean_main |>
  left_join(age_recalculation, by = c("_uuid" = "parent_uuid")) |>
  mutate(
    ind_age_below18_n = ifelse(!is.na(new_ind_age_below18_n), 
                               new_ind_age_below18_n, 
                               ind_age_below18_n),
    ind_age_above18_n = ifelse(!is.na(new_ind_age_above18_n), 
                               new_ind_age_above18_n, 
                               ind_age_above18_n)
  )
```

### Example Use Case
**Scenario:** UUID 38369a70-e3a5-4807-8618-34d03ed0a356
- **Before correction:**
  - hh_size = 6
  - ind_age_below18_n = 5
  - ind_age_above18_n = 2
  - Age sum (7) ≠ hh_size (6) ❌ **INCONSISTENT**
  
- **After removal of invalid person_id from loop sheets:**
  - Roster now shows 3 under-18, 3 above-18
  
- **After recalculation:**
  - hh_size = 6
  - ind_age_below18_n = 3 (corrected from 5)
  - ind_age_above18_n = 3 (corrected from 2)
  - Age sum (6) = hh_size (6) ✓ **RECONCILED**

---

## Section 3: Cleaning Log Creation
**Lines: 1800-1884**

### Purpose
Document all consistency corrections in standardized cleaning log format for audit trail and reproducibility.

### Log Formats

#### A. Loop Sheet Removal Log (Lines 1801-1845)
For each removed record, logs:
- **uuid:** Household identifier
- **dataset:** Which loop (Education, Health, etc.)
- **column:** "person_id"
- **old_value:** person_id being removed
- **new_value:** NA (deletion)
- **issue_type:** "Inconsistent person_id across loop sheets"
- **rationale:** Detailed explanation of why removed
- **correction_type:** "Loop sheet harmonization"

#### B. Age Indicator Correction Log (Lines 1847-1884)
For each corrected household, logs:
- **uuid:** Household identifier
- **column:** "ind_age_below18_n" or "ind_age_above18_n"
- **old_value:** Previous incorrect count
- **new_value:** Recalculated correct count
- **issue_type:** "Age demographic indicator mismatch"
- **rationale:** Explains reconciliation process
- **resolution_status:** "Recalculated and harmonized"

#### C. Combined Master Log
Both logs merged with:
- Consistent column structure
- Chronological ordering
- Clear issue categorization

---

## Section 4: Verification & Quality Checks
**Lines: 1886-1946**

### A. Age Reconciliation Verification (Lines 1890-1936)
Creates comprehensive check table showing:

| Metric | Definition | What We Check |
|--------|-----------|---------------|
| roster_below18 | Count from roster where age < 18 | Does roster age calc match? |
| roster_above18 | Count from roster where age ≥ 18 | Does roster age calc match? |
| reported_below18 | Current hh indicator value | Does it match recalculated? |
| reported_above18 | Current hh indicator value | Does it match recalculated? |
| all_reconcile | All 4 above checks pass | Final quality metric |

**Output:** Reconciliation rate as percentage
- Pre-correction: Multiple mismatches
- Post-correction: Should approach 100%

### B. Loop Sheet Consistency Verification (Lines 1938-1946)
For each loop, reports:
- Total records remaining
- Invalid person_ids found (should be 0)
- Consistency status (✓ Consistent or ⚠️ count invalid records)

---

## Section 5: Export & Logging
**Lines: 1948-2110**

### Excel Workbook Structure

#### File 1: `loop_consistency_and_age_harmonization_[DATE].xlsx`
**Purpose:** Detailed record of specific corrections made

**Sheets:**
1. **Summary** - Overview statistics
2. **Consistency Log** - All individual corrections
3. **Loop Sheet Removals** - Only if records were removed
4. **Age Indicator Corrections** - Only if indicators recalculated
5. **Verification Report** - Pre/post-correction metrics

**Column Structure (Cleaning Log Sheets):**
```
date | uuid | dataset | column | old_value | new_value | 
action | issue_type | rationale | correction_type | 
changed_by | reviewed_by | resolution_status | timestamp
```

#### File 2: `master_consistency_and_harmonization_log_[DATE].xlsx`
**Purpose:** Complete audit trail combining hh_size + consistency corrections

**Sheets:**
1. **Overview** - Summary counts by correction type
2. **Master Log** - All 56+ corrections in one table
3. **Reconciliation Report** - Pre/post data quality summary

---

## Section 6: Cleaning Log Standards Applied

All logs follow this standardized format:

| Column | Content | Example |
|--------|---------|---------|
| date | Date of correction | 2026-10-09 |
| uuid | Household unique identifier | 38369a70-e3a5... |
| dataset | Which dataset affected | Household Main, Education |
| column | Column name modified | ind_age_below18_n |
| old_value | Previous incorrect value | 5 |
| new_value | New corrected value | 3 |
| action | Type of change | changed, deleted |
| issue_type | Category of problem | Age demographic indicator mismatch |
| rationale | Detailed explanation | Full narrative of issue and solution |
| correction_type | Method used | Demographic indicator recalculation |
| changed_by | Who made the change | Script - age indicator harmonization |
| reviewed_by | Reviewer (if applicable) | NA (for scripted corrections) |
| resolution_status | Current status | Recalculated and harmonized |
| timestamp | Precise timestamp | 2026-10-09 15:30:45 |

---

## Key Assumptions & Dependencies

1. **Reference Data:**
   - clean_roster must be already cleaned and deduplicated
   - Loop sheets (edu, health, nutrition, protection) exist and have parent_uuid + person_id
   - clean_main has hh_size and age indicators

2. **Person_id Requirements:**
   - Must be unique within household
   - Consistent across all loop sheets
   - Present in both roster and relevant loops

3. **Age Data:**
   - Age field in roster is populated
   - NAs handled appropriately in sum() functions

4. **No Breaking Changes:**
   - All updates are applied to already-loaded objects in R memory
   - New dataframes not created; existing ones updated in place
   - Cleaning logs are NEW objects for export

---

## Validation Checkpoints

```
✓ Loop consistency check: anti_join identifies orphans
✓ Age calculation: sum(age<18) + sum(age>=18) = n()
✓ Reconciliation: age_sum == roster_size == hh_size
✓ Logging: All changes captured with rationale
✓ Export: Excel files created with verification sheets
```

---

## Console Output Interpretation

When script runs, you'll see:
```
=== CHECKING CONSISTENCY ACROSS INDIVIDUAL-LEVEL LOOP SHEETS ===
Created reference roster with [X] valid person records

--- Checking Education Loop ---
⚠️  Found [N] records in Education loop that don't match updated roster

--- Checking Health Loop ---
✓ All records in Health loop are valid (match roster)

[... etc for Nutrition and Child Protection ...]

=== RECALCULATING AGE-RELATED DEMOGRAPHIC INDICATORS ===
Records requiring age indicator recalculation: [M]

Age indicators reconciliation rate: 100.0%

Loop Sheet Consistency Verification:
# A tibble: 4 × 4
  dataset    n_records invalid_person_ids consistency_status
  <chr>          <int>              <int> <chr>
1 Education       1234                  0 ✓ Consistent
2 Health           543                  0 ✓ Consistent
3 Nutrition        892                  0 ✓ Consistent
4 Child Protection 234                  0 ✓ Consistent
```

---

## Common Errors & Solutions

| Error | Cause | Solution |
|-------|-------|----------|
| "person_id columns missing" | Loop sheet lacks person_id | Check column naming: should be "person_id" |
| "Mismatch still 98.2%" | Age values not updated | Check if `clean_main` reassignment worked |
| "0 records in logs" | No inconsistencies found | This is SUCCESS - all data was already consistent |
| "Excel export fails" | File locked/permissions | Close file or check write permissions |

---

## Next Steps in Pipeline

After this section completes:
1. clean_main, clean_roster, clean_edu_ind, clean_health_ind, clean_nutrition_ind, clean_child_protection_ind are fully harmonized
2. All cleaning logs are exported for audit
3. Ready for downstream analysis or final export
4. Can proceed to script 9 (Remove unnecessary variables) with confidence in data consistency

---

*Last Updated: 2026-10-09*
*Script Section: 8.Generate the clean df and review the cleaning.R (Lines 1585+)*
