# =================================================================================================
# REACH MSNA 2026 Mozambique - 1b. Outlier Detection Helper Functions
# -------------------------------------------------------------------------------------------------
# Purpose:
#   Provides utility functions for detecting outliers in continuous variables and interview duration.
#   These functions are called from Script 3 (logical checks evaluation) to dynamically identify
#   outliers in the cleaned data and flag them for field verification.
#
# Functions:
#   - detect_iqr_outliers(): Identify outliers using the IQR method for a single numeric column
#   - detect_duration_outliers(): Identify unusually short/long interview durations
#   - apply_outlier_flags(): Apply outlier flags to a dataset and return flagged rows with details
#
# Usage:
#   These are intended to be sourced and called from Script 3 after data cleaning is complete.
# =================================================================================================

# --------------------------------------------------------------------------------------------------
# Helper: Detect IQR outliers for a single numeric variable
# --------------------------------------------------------------------------------------------------
# Returns: logical vector of length nrow(x), TRUE for outliers, NA for missing values
# Method:
#   - Q1, Q3, IQR calculated on non-missing values
#   - Lower bound = Q1 - 1.5 * IQR
#   - Upper bound = Q3 + 1.5 * IQR
#   - Values outside bounds flagged as outliers; missing values remain NA

detect_iqr_outliers <- function(x) {
  # Coerce to numeric, suppress warnings for non-numeric conversions
  x_num <- suppressWarnings(as.numeric(x))
  
  # Extract non-missing values
  non_na <- x_num[!is.na(x_num)]
  
  # If fewer than 4 values or all identical, no outliers can be detected
  if (length(non_na) < 4 || sd(non_na, na.rm = TRUE) == 0) {
    return(rep(NA, length(x)))
  }
  
  # Calculate quartiles and IQR
  q1 <- quantile(non_na, 0.25, names = FALSE, type = 7)
  q3 <- quantile(non_na, 0.75, names = FALSE, type = 7)
  iqr <- q3 - q1
  
  # Bounds
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  
  # Flag: TRUE for outliers (outside bounds), FALSE for within range, NA for missing
  (x_num < lower_bound | x_num > upper_bound)
}

# --------------------------------------------------------------------------------------------------
# Helper: Detect interview duration outliers
# --------------------------------------------------------------------------------------------------
# Inputs:
#   - start_time, end_time: POSIXct or character vectors (coerced to POSIXct)
#   - method: "iqr" (default) or "fixed"
#   - min_duration_minutes, max_duration_minutes (for fixed method): bounds in minutes
#
# Returns: logical vector, TRUE for outlier durations
#
# Method:
#   IQR (default): Calculates duration in minutes, applies IQR method
#   Fixed bounds: Flags durations < min or > max minutes

detect_duration_outliers <- function(start_time, end_time, 
                                     method = "iqr",
                                     min_duration_minutes = 5,
                                     max_duration_minutes = 180) {
  
  # Coerce timestamps to POSIXct
  start <- tryCatch(as.POSIXct(start_time), error = function(e) rep(NA_real_, length(start_time)))
  end   <- tryCatch(as.POSIXct(end_time),   error = function(e) rep(NA_real_, length(end_time)))
  
  # Calculate duration in minutes
  duration_min <- as.numeric(difftime(end, start, units = "mins"))
  
  # Negative durations (end before start) are always flagged
  is_negative <- !is.na(duration_min) & duration_min < 0
  
  if (method == "fixed") {
    # Simple bounds check
    outliers <- is.na(duration_min) | duration_min < min_duration_minutes | duration_min > max_duration_minutes
  } else {
    # IQR method on valid durations
    outliers <- detect_iqr_outliers(duration_min)
  }
  
  # Always flag negative durations as outliers
  outliers[is_negative] <- TRUE
  
  return(outliers)
}

# --------------------------------------------------------------------------------------------------
# Main: Apply outlier detection to all numeric columns in a dataset
# --------------------------------------------------------------------------------------------------
# Inputs:
#   - df: data frame (cleaned data)
#   - dataset_name: name of dataset ("Submissions", "roster", "edu_ind", etc.)
#   - exclude_vars: character vector of variable names to exclude from outlier detection
#   - verbose: if TRUE, print summary of detected outliers
#
# Returns: 
#   - List with:
#     - outlier_flags: data frame with columns (uuid/key, variable, value, flag_type, details)
#     - n_rows_with_outliers: number of rows flagged
#     - is_outlier_flag: logical vector to add to df as "is_outlier" column

apply_outlier_flags <- function(df, dataset_name, 
                                exclude_vars = c("uuid", "_uuid", "start_time", "end_time", 
                                                 "submission_time", "_submission_time",
                                                 "today", "index", "point_number", "parent_index"),
                                verbose = TRUE) {
  
  if (!is.data.frame(df) || nrow(df) == 0) {
    return(list(
      outlier_flags = tibble::tibble(),
      n_rows_with_outliers = 0L,
      is_outlier_flag = rep(FALSE, nrow(df))
    ))
  }
  
  # Get numeric columns (integer/decimal types)
  numeric_cols <- names(df)[vapply(df, function(x) is.numeric(x) || is.character(x), logical(1))]
  numeric_cols <- setdiff(numeric_cols, exclude_vars)
  
  # Container for outlier detections
  outlier_details <- tibble::tibble(
    row_index = integer(),
    variable = character(),
    value = character(),
    is_outlier = logical()
  )
  
  # Special case: Submissions dataset includes interview duration check
  if (dataset_name == "Submissions" && "start_time" %in% names(df) && "end_time" %in% names(df)) {
    duration_flags <- detect_duration_outliers(df$start_time, df$end_time, method = "iqr")
    
    for (i in which(!is.na(duration_flags) & duration_flags)) {
      start <- df$start_time[[i]]
      end   <- df$end_time[[i]]
      dur_min <- as.numeric(difftime(end, start, units = "mins"))
      
      outlier_details <- dplyr::bind_rows(
        outlier_details,
        tibble::tibble(
          row_index = i,
          variable = "interview_duration_minutes",
          value = format(dur_min, digits = 1),
          is_outlier = TRUE
        )
      )
    }
  }
  
  # Check each numeric column for outliers
  for (var in numeric_cols) {
    if (!var %in% names(df)) next
    
    col_data <- df[[var]]
    flags <- detect_iqr_outliers(col_data)
    
    for (i in which(!is.na(flags) & flags)) {
      outlier_details <- dplyr::bind_rows(
        outlier_details,
        tibble::tibble(
          row_index = i,
          variable = var,
          value = as.character(col_data[[i]]),
          is_outlier = TRUE
        )
      )
    }
  }
  
  # Create is_outlier flag for entire row: TRUE if any variable in that row is flagged
  is_outlier_flag <- seq_len(nrow(df)) %in% outlier_details$row_index
  
  if (verbose && nrow(outlier_details) > 0) {
    cat("\n[Outlier Detection]", dataset_name, ":\n")
    cat("  - Rows with outliers:", sum(is_outlier_flag), "of", nrow(df), "\n")
    cat("  - Total flagged values:", nrow(outlier_details), "\n")
    if (nrow(outlier_details) <= 20) {
      cat("  - Flagged variables:", paste(unique(outlier_details$variable), collapse = ", "), "\n")
    } else {
      vars_flagged <- unique(outlier_details$variable)
      cat("  - Flagged variables (", length(vars_flagged), "):", 
          paste(head(vars_flagged, 5), collapse = ", "), "...\n", sep = "")
    }
  }
  
  return(list(
    outlier_flags = outlier_details,
    n_rows_with_outliers = sum(is_outlier_flag),
    is_outlier_flag = is_outlier_flag
  ))
}

# --------------------------------------------------------------------------------------------------
# Helper: Generate outlier summary for reporting
# --------------------------------------------------------------------------------------------------
# Summarizes detected outliers by variable and dataset for logging/documentation

summarise_outlier_detections <- function(outlier_list_by_dataset) {
  
  summary_rows <- list()
  
  for (ds in names(outlier_list_by_dataset)) {
    outlier_details <- outlier_list_by_dataset[[ds]]$outlier_flags
    
    if (nrow(outlier_details) > 0) {
      by_var <- outlier_details |>
        dplyr::group_by(variable) |>
        dplyr::summarise(n_flagged = n(), .groups = "drop") |>
        dplyr::mutate(dataset = ds)
      
      summary_rows[[ds]] <- by_var
    }
  }
  
  if (length(summary_rows) == 0) {
    return(tibble::tibble(dataset = character(), variable = character(), n_flagged = integer()))
  }
  
  dplyr::bind_rows(summary_rows) |>
    dplyr::arrange(dataset, dplyr::desc(n_flagged))
}
