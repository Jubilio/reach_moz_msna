# Install required packages for REACH MSNA 2026 Uganda cleaning pipeline
required_packages <- c("tidyverse", "readxl", "openxlsx", "writexl", "lubridate", "fs", "crayon", "janitor", "stringr", "purrr", "rlang", "tidyselect", "naniar")
optional_packages <- c("cleaningtools", "humind")
missing_required <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_required) > 0) install.packages(missing_required)
missing_optional <- optional_packages[!vapply(optional_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_optional) > 0) message("Optional packages not installed: ", paste(missing_optional, collapse = ", "), ". Install them from their usual source if your REACH setup uses them.")
