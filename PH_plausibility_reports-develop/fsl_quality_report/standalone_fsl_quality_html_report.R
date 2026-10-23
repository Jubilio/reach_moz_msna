# Please adapt with your own dataset and values.
# FSL indicators must be standardized with impactR4PHU before using generating the run_fsl_plaus_html_report function.

library(dplyr)
library(purrr)
library(fs)
library(scales)
library(openxlsx)
library(devtools)
library(here)
library(readxl)

# install.packages("devtools")
# devtools::install_github("impact-initiatives/impactR4PHU")



fs::dir_ls(here::here("R"), recurse = TRUE, glob = "*.R") |>
  purrr::walk(
    ~ source(.x)
  )


data.test <- readxl::read_xlsx(
  here::here("fsl_quality_report", "inputs", "REACH_MOZ_Data_MSNA_01102026.xlsx"),
  sheet = "MOZ 2026 MSNA Kobo Tool FINAL"
)


table(data.test$fsl_lcsi_stress1)

# Set Parameters


loop_var <- "admin1"        # Variável para iterar os relatórios (ex.: Província)
grouping_var <- "admin2"    # Variável secundária (ex.: Distrito ou enum_id)
uuidVar <- "uuid"           # Identificador único do agregado

# 


# Add FSL Indicators for Plausibility

data.test2 <- data.test %>%
  dplyr::select(!ends_with("_recoded")) %>%
  impactR4PHU::add_rcsi(
    fsl_rcsi_mealnb = "fsl_rcsi_mealnb",
    fsl_rcsi_mealsize = "fsl_rcsi_mealsize",
    fsl_rcsi_mealadult = "fsl_rcsi_mealadult",
    fsl_rcsi_lessquality = "fsl_rcsi_rely_on_less_expensive_food",
    fsl_rcsi_borrow = "fsl_rcsi_borrow"
  ) %>%
  impactR4PHU::add_hhs(
    fsl_hhs_nofoodhh = "m2754_hh_hhs_no_food",
    fsl_hhs_nofoodhh_freq = "m2755_hh_hhs_no_food_frequency",
    fsl_hhs_sleephungry = "m2756_hh_hhs_sleep_hungry",
    fsl_hhs_sleephungry_freq = "m2757_hh_hhs_sleep_hungry_frequency",
    fsl_hhs_alldaynight = "m2758_hh_hhs_no_day_night",
    fsl_hhs_alldaynight_freq = "m2759_hh_hhs_no_day_night_frequency",
    yes_answer = "yes",
    no_answer = "no",
    rarely_answer = "rarely",
    sometimes_answer = "sometimes",
    often_answer = "often"
  ) %>%
  impactR4PHU::add_fcs(
    fsl_fcs_cereal = "m3269_hh_fcs_staples",
    fsl_fcs_legumes = "m3270_hh_fcs_pulses",
    fsl_fcs_veg = "m3273_hh_fcs_veg",
    fsl_fcs_dairy = "m3271_hh_fcs_dairy",
    fsl_fcs_fruit = "m3274_hh_fcs_fruits",
    fsl_fcs_meat = "m3272_hh_fcs_protein",
    fsl_fcs_sugar = "m3276_hh_fcs_sugar",
    fsl_fcs_oil = "m3275_hh_fcs_fat",
    cutoffs = "normal"
  ) %>%
  impactR4PHU::add_fcm_phase(
    fcs_column_name = "fsl_fcs_cat",
    rcsi_column_name = "fsl_rcsi_cat",
    hhs_column_name = "fsl_hhs_cat_ipc",
    hdds_column_name = "fsl_hdds_cat",
    fcs_categories_acceptable = "Acceptable",
    fcs_categories_poor = "Poor",
    fcs_categories_borderline = "Borderline",
    rcsi_categories_low = "No to Low",
    rcsi_categories_medium = "Medium",
    rcsi_categories_high = "High",
    hhs_categories_none = "None",
    hhs_categories_little = "Little",
    hhs_categories_moderate = "Moderate",
    hhs_categories_severe = "Severe",
    hhs_categories_very_severe = "Very Severe",
    hdds_categories_low = "Low",
    hdds_categories_medium = "Medium",
    hdds_categories_high = "High"
  ) %>%
  impactR4PHU::add_lcsi(
    fsl_lcsi_stress1 = "fsl_lcsi_stress1",
    fsl_lcsi_stress2 = "fsl_lcsi_stress2",
    fsl_lcsi_stress3 = "fsl_lcsi_stress3",
    fsl_lcsi_stress4 = "fsl_lcsi_stress4",
    fsl_lcsi_crisis1 = "fsl_lcsi_crisis1",
    fsl_lcsi_crisis2 = "fsl_lcsi_crisis2", 
    fsl_lcsi_crisis3 = "fsl_lcsi_crisis3",
    fsl_lcsi_emergency1 = "fsl_lcsi_emergency1",
    fsl_lcsi_emergency2 = "fsl_lcsi_emergency2",
    fsl_lcsi_emergency3 = "fsl_lcsi_emergency3",
    yes_val = "yes",
    no_val = "no_had_no_need",
    exhausted_val = "no_exhausted",
    not_applicable_val = "not_applicable"
  ) %>%
  dplyr::rename(
    "fsl_lcsi_stress1" = "fsl_lcsi_stress1",
    "fsl_lcsi_stress2" = "fsl_lcsi_stress2",
    "fsl_lcsi_stress3" = "fsl_lcsi_stress3",
    "fsl_lcsi_stress4" = "fsl_lcsi_stress4",
    "fsl_lcsi_crisis1" = "fsl_lcsi_crisis1",
    "fsl_lcsi_crisis2" = "fsl_lcsi_crisis2",
    "fsl_lcsi_crisis3" = "fsl_lcsi_crisis3",
    "fsl_lcsi_emergency1" = "fsl_lcsi_emergency1",
    "fsl_lcsi_emergency2" = "fsl_lcsi_emergency2",
    "fsl_lcsi_emergency3" = "fsl_lcsi_emergency3"
  ) %>%
  impactR4PHU::check_fsl_flags(
    fsl_hhs_nofoodhh          = "fsl_hhs_nofoodhh",
    fsl_hhs_nofoodhh_freq     = "fsl_hhs_nofoodhh_freq",
    fsl_hhs_sleephungry       = "fsl_hhs_sleephungry",
    fsl_hhs_sleephungry_freq  = "fsl_hhs_sleephungry_freq",
    fsl_hhs_alldaynight       = "fsl_hhs_alldaynight",
    fsl_hhs_alldaynight_freq  = "fsl_hhs_alldaynight_freq",
    fsl_lcsi_stress           = "fsl_lcsi_stress",
    fsl_lcsi_crisis           = "fsl_lcsi_crisis",
    fsl_lcsi_emergency        = "fsl_lcsi_emergency",
    fsl_lcsi_cat_yes          = "fsl_lcsi_cat_yes",
    fsl_lcsi_cat_exhaust      = "fsl_lcsi_cat_exhaust",
    fsl_lcsi_cat              = "fsl_lcsi_cat",
    uuid                      = "_uuid"
  )


run_fsl_plaus_html_report <- function(
  .dataset = NULL,
  uuid_var = "_uuid",
  group_var = "enum_id",
  output_file = "fsl_quality_report.pdf",
  output_dir = "reports"
) {
  if (is.null(.dataset)) {
    stop("Error: 'dataset_main' must be provided.")
  }
  if (is.null(uuid_var)) {
    stop("Error: 'uuid' variable must be provided.")
  }
  if (is.null(group_var)) {
    stop("Error: 'group' var must be provided.")
  }
  rmarkdown::render(
    input = here::here("fsl_quality_report", "fsl_quality_report_markdown.Rmd"),
    output_file = output_file,
    output_dir = output_dir,
    params = list(
      mainData = .dataset,
      uuidVar = uuid_var,
      GroupVar = group_var
    ),
  )
}


loop_values <- unique(data.test2[[loop_var]])


reports_path <- here::here("fsl_quality_report", "reports")
fs::dir_create(reports_path)

for (i in 1:length(loop_values)) {
  print(loop_values[[i]])

  file_name <- paste0(
    "fsl_plaus_report_",
    loop_values[[i]],
    "_",
    Sys.Date(),
    ".html"
  )

  data.test3 <- data.test2 %>%
    dplyr::filter(!!sym(loop_var) == loop_values[[i]])

  run_fsl_plaus_html_report(
    .dataset = data.test3,
    uuid_var = uuidVar,
    group_var = grouping_var,
    output_dir = reports_path,
    output_file = file_name
  )
}





