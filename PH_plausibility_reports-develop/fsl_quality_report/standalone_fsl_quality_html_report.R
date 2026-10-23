# =============================================================================
# FSL Plausibility Report - Mozambique MSNA 2026
# =============================================================================
# Este script utiliza a base processada gerada pelo pipeline de Moçambique:
#   R-scripts/moz/run.R
# =============================================================================

library(dplyr)
library(purrr)
library(fs)
library(scales)
library(openxlsx)
library(devtools)
library(here)
library(readxl)

# Configurar Pandoc para renderização do RMarkdown (caso executado fora do RStudio)
if (Sys.getenv("RSTUDIO_PANDOC") == "") {
  rstudio_pandoc <- "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools"
  if (file.exists(rstudio_pandoc)) {
    Sys.setenv(RSTUDIO_PANDOC = rstudio_pandoc)
  }
}

# Funções utilitárias locais
fs::dir_ls(here::here("R"), recurse = TRUE, glob = "*.R") |>
  purrr::walk(~ source(.x))

# ---- 1. Carregar os dados processados da execução mais recente de Moçambique ----
moz_runs <- sort(fs::dir_ls(here::here("..", "outputs", "moz"), type = "directory"))
if (length(moz_runs) == 0) {
  stop("Nenhuma saída encontrada em outputs/moz/.\nExecute 'source(\"R-scripts/moz/run.R\")' a partir da raiz do projeto primeiro.")
}
latest_run <- tail(moz_runs, 1)
message("Carregando dados da pasta: ", latest_run)

data.test <- read.csv(file.path(latest_run, "Submissions.csv"), check.names = FALSE, stringsAsFactors = FALSE)

# Anexar colunas de frequência do HHS a partir do raw_snapshot
raw_snap <- readRDS(file.path(latest_run, "raw_snapshot.rds"))
subm_raw <- raw_snap$Submissions

data.test$m2755_hh_hhs_no_food_frequency       <- subm_raw[[151]]
data.test$m2757_hh_hhs_sleep_hungry_frequency  <- subm_raw[[153]]
data.test$m2759_hh_hhs_no_day_night_frequency  <- subm_raw[[155]]

# ---- 2. Parâmetros de Agrupamento ------------------------------------------
loop_var     <- "admin1"   # Variável para iterar os relatórios (ex.: Província)
grouping_var <- "admin2"   # Variável de agregação no relatório (ex.: Distrito)
uuidVar      <- "uuid"     # Identificador único do agregado

# ---- 3. Padronização das variáveis FSL (mapeamento Moçambique -> impactR4PHU) -
# FCS (Food Consumption Score)
data.test$fsl_fcs_cereal  <- suppressWarnings(as.numeric(data.test$m3269_hh_fcs_staples))
data.test$fsl_fcs_legumes <- suppressWarnings(as.numeric(data.test$m3270_hh_fcs_pulses))
data.test$fsl_fcs_veg     <- suppressWarnings(as.numeric(data.test$m3273_hh_fcs_veg))
data.test$fsl_fcs_dairy   <- suppressWarnings(as.numeric(data.test$m3271_hh_fcs_dairy))
data.test$fsl_fcs_fruit   <- suppressWarnings(as.numeric(data.test$m3274_hh_fcs_fruits))
data.test$fsl_fcs_meat    <- suppressWarnings(as.numeric(data.test$m3272_hh_fcs_protein))
data.test$fsl_fcs_sugar   <- suppressWarnings(as.numeric(data.test$m3276_hh_fcs_sugar))
data.test$fsl_fcs_oil     <- suppressWarnings(as.numeric(data.test$m3275_hh_fcs_fat))

# rCSI (reduced Coping Strategies Index)
data.test$fsl_rcsi_lessquality <- suppressWarnings(as.numeric(data.test$fsl_rcsi_rely_on_less_expensive_food))
data.test$fsl_rcsi_borrow      <- suppressWarnings(as.numeric(data.test$fsl_rcsi_borrow))
data.test$fsl_rcsi_mealsize    <- suppressWarnings(as.numeric(data.test$fsl_rcsi_mealsize))
data.test$fsl_rcsi_mealadult   <- suppressWarnings(as.numeric(data.test$fsl_rcsi_mealadult))
data.test$fsl_rcsi_mealnb      <- suppressWarnings(as.numeric(data.test$fsl_rcsi_mealnb))

# HHS (Household Hunger Scale)
data.test$fsl_hhs_nofoodhh         <- data.test$m2754_hh_hhs_no_food
data.test$fsl_hhs_nofoodhh_freq    <- data.test$m2755_hh_hhs_no_food_frequency
data.test$fsl_hhs_sleephungry      <- data.test$m2756_hh_hhs_sleep_hungry
data.test$fsl_hhs_sleephungry_freq <- data.test$m2757_hh_hhs_sleep_hungry_frequency
data.test$fsl_hhs_alldaynight      <- data.test$m2758_hh_hhs_no_day_night
data.test$fsl_hhs_alldaynight_freq <- data.test$m2759_hh_hhs_no_day_night_frequency

# ---- 4. Recodificar LCSI de Moçambique para o formato do impactR4PHU -------
# Códigos no formulário XLSForm de Moçambique (lista l_fsl_lcsi):
#   30   = Sim (Yes)
#   10   = Não, não tinha necessidade (No, had no need)
#   20   = Não, já esgotei a estratégia (No, exhausted)
#   9999 = Não aplicável (Not applicable)
rec_lcsi <- function(x) {
  x <- as.character(x)
  dplyr::case_when(
    x == "30"   ~ "yes",
    x == "10"   ~ "no_had_no_need",
    x == "20"   ~ "no_exhausted",
    x == "9999" ~ "not_applicable",
    TRUE        ~ "not_applicable"
  )
}

data.test$fsl_lcsi_stress1 <- rec_lcsi(data.test$fsl_lcsi_send_family_members_elsewhere)
data.test$fsl_lcsi_stress2 <- rec_lcsi(data.test$fsl_lcsi_sell_more_reproductive_animals)
data.test$fsl_lcsi_stress3 <- "not_applicable"
data.test$fsl_lcsi_stress4 <- "not_applicable"

data.test$fsl_lcsi_crisis1 <- rec_lcsi(data.test$fsl_lcsi_reduce_expenses_on_health)
data.test$fsl_lcsi_crisis2 <- "not_applicable"
data.test$fsl_lcsi_crisis3 <- "not_applicable"

data.test$fsl_lcsi_emergency1 <- "not_applicable"
data.test$fsl_lcsi_emergency2 <- "not_applicable"
data.test$fsl_lcsi_emergency3 <- "not_applicable"

# ---- 5. Cálculo dos Indicadores FSL -----------------------------------------
data.test2 <- data.test %>%
  dplyr::select(!ends_with("_recoded")) %>%

  # -- rCSI (reduced Coping Strategies Index) --
  impactR4PHU::add_rcsi(
    fsl_rcsi_lessquality = "fsl_rcsi_lessquality",
    fsl_rcsi_borrow      = "fsl_rcsi_borrow",
    fsl_rcsi_mealsize    = "fsl_rcsi_mealsize",
    fsl_rcsi_mealadult   = "fsl_rcsi_mealadult",
    fsl_rcsi_mealnb      = "fsl_rcsi_mealnb"
  ) %>%

  # -- HHS (Household Hunger Scale) --
  impactR4PHU::add_hhs(
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
  ) %>%

  # -- FCS (Food Consumption Score) --
  impactR4PHU::add_fcs(
    fsl_fcs_cereal  = "fsl_fcs_cereal",
    fsl_fcs_legumes = "fsl_fcs_legumes",
    fsl_fcs_veg     = "fsl_fcs_veg",
    fsl_fcs_dairy   = "fsl_fcs_dairy",
    fsl_fcs_fruit   = "fsl_fcs_fruit",
    fsl_fcs_meat    = "fsl_fcs_meat",
    fsl_fcs_sugar   = "fsl_fcs_sugar",
    fsl_fcs_oil     = "fsl_fcs_oil",
    cutoffs         = "normal"
  )

# -- FCM Phase (Food Consumption Matrix) --
if (!"fsl_hdds_cat" %in% names(data.test2)) {
  data.test2$fsl_hdds_cat <- NA_character_
}

data.test2 <- data.test2 %>%
  impactR4PHU::add_fcm_phase(
    fcs_column_name  = "fsl_fcs_cat",
    rcsi_column_name = "fsl_rcsi_cat",
    hhs_column_name  = "fsl_hhs_cat_ipc",
    hdds_column_name = "fsl_hdds_cat",
    fcs_categories_acceptable  = "Acceptable",
    fcs_categories_poor        = "Poor",
    fcs_categories_borderline  = "Borderline",
    rcsi_categories_low        = "No to Low",
    rcsi_categories_medium     = "Medium",
    rcsi_categories_high       = "High",
    hhs_categories_none        = "None",
    hhs_categories_little      = "Little",
    hhs_categories_moderate    = "Moderate",
    hhs_categories_severe      = "Severe",
    hhs_categories_very_severe = "Very Severe",
    hdds_categories_low        = "Low",
    hdds_categories_medium     = "Medium",
    hdds_categories_high       = "High"
  ) %>%

  # -- LCSI (Livelihood Coping Strategies) --
  impactR4PHU::add_lcsi(
    fsl_lcsi_stress1    = "fsl_lcsi_stress1",
    fsl_lcsi_stress2    = "fsl_lcsi_stress2",
    fsl_lcsi_stress3    = "fsl_lcsi_stress3",
    fsl_lcsi_stress4    = "fsl_lcsi_stress4",
    fsl_lcsi_crisis1    = "fsl_lcsi_crisis1",
    fsl_lcsi_crisis2    = "fsl_lcsi_crisis2",
    fsl_lcsi_crisis3    = "fsl_lcsi_crisis3",
    fsl_lcsi_emergency1 = "fsl_lcsi_emergency1",
    fsl_lcsi_emergency2 = "fsl_lcsi_emergency2",
    fsl_lcsi_emergency3 = "fsl_lcsi_emergency3",
    yes_val             = "yes",
    no_val              = "no_had_no_need",
    exhausted_val       = "no_exhausted",
    not_applicable_val  = "not_applicable"
  ) %>%
  impactR4PHU::check_fsl_flags(
    fsl_fcs_cereal           = "fsl_fcs_cereal",
    fsl_fcs_legumes          = "fsl_fcs_legumes",
    fsl_fcs_dairy            = "fsl_fcs_dairy",
    fsl_fcs_meat             = "fsl_fcs_meat",
    fsl_fcs_veg              = "fsl_fcs_veg",
    fsl_fcs_fruit            = "fsl_fcs_fruit",
    fsl_fcs_oil              = "fsl_fcs_oil",
    fsl_fcs_sugar            = "fsl_fcs_sugar",
    fsl_rcsi_lessquality     = "fsl_rcsi_lessquality",
    fsl_rcsi_borrow          = "fsl_rcsi_borrow",
    fsl_rcsi_mealsize        = "fsl_rcsi_mealsize",
    fsl_rcsi_mealadult       = "fsl_rcsi_mealadult",
    fsl_rcsi_mealnb          = "fsl_rcsi_mealnb",
    fsl_hhs_nofoodhh         = "fsl_hhs_nofoodhh",
    fsl_hhs_nofoodhh_freq    = "fsl_hhs_nofoodhh_freq",
    fsl_hhs_sleephungry      = "fsl_hhs_sleephungry",
    fsl_hhs_sleephungry_freq = "fsl_hhs_sleephungry_freq",
    fsl_hhs_alldaynight      = "fsl_hhs_alldaynight",
    fsl_hhs_alldaynight_freq = "fsl_hhs_alldaynight_freq",
    fsl_lcsi_stress1         = "fsl_lcsi_stress1",
    fsl_lcsi_stress2         = "fsl_lcsi_stress2",
    fsl_lcsi_stress3         = "fsl_lcsi_stress3",
    fsl_lcsi_stress4         = "fsl_lcsi_stress4",
    fsl_lcsi_crisis1         = "fsl_lcsi_crisis1",
    fsl_lcsi_crisis2         = "fsl_lcsi_crisis2",
    fsl_lcsi_crisis3         = "fsl_lcsi_crisis3",
    fsl_lcsi_emergency1      = "fsl_lcsi_emergency1",
    fsl_lcsi_emergency2      = "fsl_lcsi_emergency2",
    fsl_lcsi_emergency3      = "fsl_lcsi_emergency3",
    fsl_lcsi_stress          = "fsl_lcsi_stress",
    fsl_lcsi_crisis          = "fsl_lcsi_crisis",
    fsl_lcsi_emergency       = "fsl_lcsi_emergency",
    fsl_lcsi_cat_yes         = "fsl_lcsi_cat_yes",
    fsl_lcsi_cat_exhaust     = "fsl_lcsi_cat_exhaust",
    fsl_lcsi_cat             = "fsl_lcsi_cat",
    uuid                     = "uuid"
  )

# ---- 6. Renderizar Relatórios HTML ------------------------------------------
run_fsl_plaus_html_report <- function(
  .dataset = NULL,
  uuid_var = "uuid",
  group_var = "admin2",
  output_file = "fsl_quality_report.html",
  output_dir = "reports"
) {
  if (is.null(.dataset)) stop("Erro: '.dataset' deve ser fornecido.")
  if (is.null(uuid_var))  stop("Erro: 'uuid_var' deve ser fornecido.")
  if (is.null(group_var)) stop("Erro: 'group_var' deve ser fornecido.")
  rmarkdown::render(
    input = here::here("fsl_quality_report", "fsl_quality_report_markdown.Rmd"),
    output_file = output_file,
    output_dir  = output_dir,
    params = list(
      mainData = .dataset,
      uuidVar  = uuid_var,
      GroupVar = group_var
    )
  )
}

loop_values <- unique(data.test2[[loop_var]])
loop_values <- loop_values[!is.na(loop_values) & nzchar(trimws(loop_values))]

reports_path <- here::here("fsl_quality_report", "reports")
fs::dir_create(reports_path)

message("Gerando relatórios para as seguintes províncias: ", paste(loop_values, collapse = ", "))

for (i in seq_along(loop_values)) {
  prov <- loop_values[[i]]
  message("-> Gerando relatório FSL para: ", prov)

  file_name <- paste0("fsl_plaus_report_", gsub("[^A-Za-z0-9_-]", "_", prov), "_", Sys.Date(), ".html")
  data_subset <- data.test2 %>% dplyr::filter(!!sym(loop_var) == prov)

  run_fsl_plaus_html_report(
    .dataset    = data_subset,
    uuid_var    = uuidVar,
    group_var   = grouping_var,
    output_dir  = reports_path,
    output_file = file_name
  )
}
message("Relatórios FSL gerados com sucesso em: ", reports_path)
