# Mozambique MSNA 2026 — Integrated Analysis Module
### Education Sector (EduAnalysisTool) & Multi-Sector Needs Index (Humind MSNI)

This module implements the integrated analytical workflow for the **2026 Mozambique Multi-Sector Needs Assessment (MSNA)**, combining two core standardized methodologies from **IMPACT Initiatives / HPPU**:
1. **[EduAnalysisTool (martivit/IMPACT)](https://github.com/martivit/EduAnalysisTool)**: Framework for education sector analysis at individual and household levels.
2. **[Humind MSNI Workflow](https://impact-initiatives-hppu.github.io/humind/articles/msni-workflow.html)**: Standardized framework for calculating sectoral Living Standards Gaps (LSGs) and the overall Multi-Sector Needs Index (MSNI).

---

## 📁 Repository Structure

```
analysis/
├── config/
│   ├── config.R                     # Project settings, thresholds, and disaggregation paths
│   ├── moz_isced_mapping.csv        # Mozambique National Education System (SNE) grade mapping
│   ├── lcsi_strategies.csv          # Mapping of 11 Livelihood Coping Strategies (LCSI)
│   └── variable_labels_en.csv       # Dictionary mapping raw variables to human-readable English labels
├── R/
│   ├── 00_utils.R                   # Data loading helpers and English label dictionary applicator
│   ├── 01_edu_indicators.R          # Education indicator calculation engine (EduAnalysisTool & Humind)
│   ├── 02_msni_indicators.R         # Sectoral composites (LSG) & MSNI calculation engine (Humind)
│   ├── 03_analysis_engine.R         # Statistical aggregation engine across disaggregation dimensions
│   └── 04_export_outputs.R          # Export engine for formatted Excel (.xlsx), HTML report, and RDS
├── reports/
│   └── msna_moz_2026_analysis_report.Rmd # Interactive HTML report template (English, Bootstrap theme)
├── run_analysis.R                   # Master execution script (single entry point)
├── output/                          # Output deliverables directory (gitignored for data protection)
└── README.md                        # Module documentation
```

---

## 🧠 Methodological Framework

### 1. Education Sector Assessment (EduAnalysisTool)
Individual analysis is conducted over school-age children (**ages 5 to 17**, $N = 34,725$) from the `edu_ind` loop, and aggregated to the household level:
- **School-Age Brackets**:
  - `5 years old`: Early Childhood Education (ECE) / Entrance age for Grade 1.
  - `6 to 11 years old`: Primary School (Grades 1 to 6).
  - `12 to 15 years old`: Lower Secondary School (Grades 7 to 10).
  - `16 to 17 years old`: Upper Secondary School (Grades 11 and 12).
- **Access & Out-of-School Rates**:
  - Current school year attendance and out-of-school (OOS / OSC) rate.
- **Over-Age Attendance**:
  - Enrolled students who are $\ge 2$ years older than the intended age for their grade (based on `moz_isced_mapping.csv`).
- **Education Disruption Events**:
  - Natural hazards (cyclones, floods, droughts).
  - Teacher absences and strikes.
  - Schools used as shelters for displaced populations.
  - Direct conflict / security attacks on education.
- **Reported Barriers to School Attendance**:
  - Financial costs (tuition fees, uniforms, materials, transportation).
  - Distance to school.
  - Safety and security concerns during the journey.
  - Early marriage and adolescent pregnancy.
  - Lack of civil documentation / birth certificate.
  - Disability or chronic health condition.
  - School closure.

### 2. Sectoral Living Standards Gaps & MSNI (Humind)
Each household receives a severity score from 1 to 4+ across six humanitarian sectors:
- **Food Security & Livelihoods (`comp_foodsec_score`)**: FCM/FCLCM matrix combining FCS, rCSI, HHS, and local LCSI strategies.
- **WASH (`comp_wash_score`)**: WHO/UNICEF JMP ladder for drinking water, collection time, per capita volume (<15 L/person/day), and sanitation.
- **Education (`comp_edu_score`)**: Presence of out-of-school children, severe disruptions, and protection-related barriers.
- **Health (`comp_health_score`)**: Unmet healthcare needs within the household.
- **Shelter & NFI (`comp_snfi_score`)**: Severe structural damage, makeshift shelters, tents, and collective centres.
- **Protection (`comp_prot_score`)**: Separated children, child labour, and severe violence/discrimination concerns.
- **Multi-Sector Needs Index (`msni_score`)**:
  - $$\text{MSNI} = \max(\text{LSG}_{\text{FoodSec}}, \text{LSG}_{\text{WASH}}, \text{LSG}_{\text{Edu}}, \text{LSG}_{\text{Health}}, \text{LSG}_{\text{SNFI}}, \text{LSG}_{\text{Prot}})$$
  - **In Need**: $\text{MSNI} \ge 3$
  - **In Severe Need**: $\text{MSNI} \ge 4$
  - **Co-occurrence**: Count of sectors where household is in need ($\ge 3$).

---

## 📊 Disaggregation Plans

Summary tables are automatically produced across:
1. **National Total** (`National`)
2. **Provincial Level** (`admin1` — 11 provinces: `MZ01` to `MZ11`)
3. **District Level** (`admin2`)
4. **Population Group / Displacement Status** (`population_group` — IDPs vs. Host Community)
5. **Head of Household Gender** (`hoh_gender`)
6. **Cross-Tabulations for Education**: Crossed by **Child Gender** and **School-Age Bracket**.

---

## 🚀 How to Run

### Option A: Inside RStudio (Recommended)
1. Open [`analysis/run_analysis.R`](run_analysis.R).
2. Click **Source** (or press `Ctrl + Shift + Enter`).

### Option B: Command Line / PowerShell
```powershell
& "C:\Program Files\R\R-4.6.0\bin\Rscript.exe" "analysis\run_analysis.R"
```

---

## 📦 Output Deliverables

All deliverables are saved in `analysis/output/`:
1. **Interactive HTML Report**: `MSNA_2026_MOZ_Analysis_Report_<timestamp>.html`
   - Self-contained report with executive KPI cards, ggplot figures, and searchable DT datatables in English.
2. **Formatted Excel Workbook**: `MSNA_2026_MOZ_Analysis_Results_<timestamp>.xlsx`
   - Formatted worksheets with human-readable English headers and a `Variable_Dictionary` reference tab.
3. **Enriched RDS Dataset**: `MSNA_2026_MOZ_Analyzed_Data_<timestamp>.rds`
   - Contains complete datasets with all computed indicators for GIS mapping and advanced statistical modeling.
