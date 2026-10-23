# Mozambique MSNA 2026 data cleaning

The original `R-scripts/Master cleaning script.R` is a legacy workflow from another mission. Its trackers, logical-check files, paths and sector assumptions are not directly interchangeable with the Mozambique survey. Those scripts are retained as reference.

Use the independent Mozambique entry point below. It uses the attached Mozambique XLSForm at runtime and does not require external logical-check workbooks, `cleaningtools`, Kobo credentials, or the public-health report environment.

## Run in RStudio

1. Open `MSNA_cleaning_2026.Rproj`.
2. Install dependencies once:

```r
install.packages(c('readxl', 'openxlsx'))
```

3. Copy your local input files to:
   - `inputs/kobo-used.xlsx`
   - `inputs/REACH_MOZ_Data_MSNA_01102026.xlsx`

   Alternatively edit `config/moz.R` to use the actual paths, including suffixes such as `(1)` or `(2)`.

4. Run:

```r
source('R-scripts/moz/run.R')
```

Command-line alternative:

```sh
Rscript R-scripts/moz/run.R /path/to/tool.xlsx /path/to/data.xlsx
```

## What this initial adaptation does

- Imports the household sheet and all seven configured repeat sheets, including the truncated Excel protection sheet names.
- Keeps a raw snapshot and the original answer values before approved feedback.
- Maps variable names and unique Portuguese/English labels, including 255-character labels and split multiple-choice columns. Ambiguous labels remain unchanged and are listed in `column_mapping.csv`. No positional guessing or automatic mapping of numbered `If other` labels.
- Normalizes recognized choice labels to XLSForm codes. Multiple-choice answers become semicolon-separated codes, using full dictionary labels rather than splitting labels at spaces or commas. Unknown/ambiguous answers remain unchanged and are flagged.
- Resolves administrative choice labels with parent context where the form specifies a single parent filter.
- Checks missing/duplicate household UUIDs, parent indexes and UUID/Id agreement, duplicate person links, sector-to-roster person links, household size versus roster size, numeric types, negative values against explicit form sentinel codes, ages, FCS/rCSI day ranges and consent.
- Exports an Excel review log, CSV issue details/summary, separate candidate CSVs and RDS datasets, a corrections template, applied corrections and a run manifest with input hashes.

The exported data are **candidates for review**, not a final approved clean database. All source records are retained. Review unresolved column mappings before analysing the candidate data. A literal `-999` allowed by an XLSForm constraint is preserved; it must not be treated as an actual age in analysis.

This is a first cleaning stage. General XLSForm relevance/constraints, survey-specific cross-sector checks, geographic boundary checks, FCS/HHS/LCSI recalculation, outlier review, anonymisation and sampling weights are not implemented here. The legacy plausibility reports are not run automatically. The output contains sensitive survey data and belongs in restricted local storage.

## Apply approved corrections

Use `corrections_template.csv` from the first run. Save the completed UTF-8 CSV as `inputs/feedback/moz_corrections.csv` and rerun from the same raw export and XLSForm.

| Column | Required value |
| --- | --- |
| dataset | Internal dataset name, e.g. `roster` or `Submissions` |
| record_key | Original sheet `_index`, as shown in the issue log |
| variable | Mapped variable or exact retained header |
| old_value | Original answer from `mapped_before_feedback.rds`, before choice normalization |
| new_value | Approved replacement |
| action | `replace` or `set_missing` |
| approved | `yes` to apply; all other values remain unapplied |
| reviewer | Person approving the correction |
| reason | Evidence/reason for the correction |

Corrections stop on missing targets, stale old values, duplicate targets or identifier changes. They do not delete records. Record keys are local to one export: if you re-export from Kobo, recheck the keys and old values before reusing feedback. Start each run from raw inputs; do not use a previous candidate dataset as input.

## Verify

```sh
Rscript tests/test_moz.R
```

Every run writes a separate timestamped folder under `outputs/moz`. Review the `mapping` and `issues` sheets in `review_log.xlsx` and resolve flagged records before publishing analysis. Input spreadsheets, feedback, run outputs and survey records must not be committed to this public repository.
