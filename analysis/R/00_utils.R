# =============================================================================
# Utility Functions — Data Loading and Pipeline Helpers
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(fs)
  library(here)
})

#' Localiza e carrega a base de dados mais recente processada pelo pipeline de limpeza
load_latest_clean_data <- function(outputs_dir = here::here("outputs", "moz")) {
  moz_runs <- sort(fs::dir_ls(outputs_dir, type = "directory"))
  if (length(moz_runs) == 0) {
    stop("Nenhuma saída encontrada em: ", outputs_dir,
         "\nExecute primeiro 'source(\"R-scripts/moz/run.R\")'.")
  }
  latest_run <- tail(moz_runs, 1)
  message("--> Carregando dados da execução mais recente: ", latest_run)

  rds_path <- file.path(latest_run, "candidate_data.rds")
  if (!file.exists(rds_path)) {
    stop("Arquivo 'candidate_data.rds' não encontrado em: ", latest_run)
  }

  data_tables <- readRDS(rds_path)

  # Resgatar frequências do HHS a partir do raw_snapshot caso não existam em Submissions
  raw_snap_path <- file.path(latest_run, "raw_snapshot.rds")
  if (file.exists(raw_snap_path)) {
    raw_snap <- readRDS(raw_snap_path)
    subm_raw <- raw_snap$Submissions
    subm <- data_tables$Submissions

    if (!"m2755_hh_hhs_no_food_frequency" %in% names(subm) && ncol(subm_raw) >= 155) {
      subm$m2755_hh_hhs_no_food_frequency      <- subm_raw[[151]]
      subm$m2757_hh_hhs_sleep_hungry_frequency <- subm_raw[[153]]
      subm$m2759_hh_hhs_no_day_night_frequency <- subm_raw[[155]]
      data_tables$Submissions <- subm
    }
  }

  return(data_tables)
}

#' Conversor numérico seguro (suprime warnings e converte não-finitos para NA)
to_numeric <- function(x) {
  num_x <- suppressWarnings(as.numeric(as.character(x)))
  num_x[!is.finite(num_x)] <- NA_real_
  num_x
}

#' Máscara para variáveis de 0 a 7 dias (FCS e rCSI)
mask_days <- function(x) {
  val <- to_numeric(x)
  val[val < 0 | val > 7 | val != floor(val)] <- NA_real_
  val
}

#' Verifica se todas as colunas necessárias existem no data.frame
assert_columns <- function(df, required_cols, df_name = "dataset") {
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    warning("Colunas ausentes em '", df_name, "': ", paste(missing_cols, collapse = ", "))
    return(FALSE)
  }
  return(TRUE)
}
