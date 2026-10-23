cat("\014")

library(dplyr)
library(stringr)
library(tibble)

# ============================================================
# 1. Track data-frame and list objects before running the code
# ============================================================

all_dfs_names_before <- Filter(
  function(x) is(x, "data.frame"),
  mget(ls())
) |>
  names()

all_lists_names_before <- Filter(
  function(x) is(x, "list"),
  mget(ls())
) |>
  names()






# creating the combined cleaning log without the removed surveys 
cleaning_log_no_dl <- cleaning_log |> filter(change_type != "remove_survey")
deletion_log <- cleaning_log |> filter(change_type == "remove_survey") 







pii_cols <- c(
  "gps",
  "_gps_latitude",
  "_gps_longitude",
  "_gps_altitude",
  "_gps_precision",
  "attendant_phone",
  "meta/rootUuid",
  "_submitted_by",
  "audit",
  "audit_URL",
  "pt_sample_lat",
  "pt_sample_lon",
  "geopoint",
  "hh_registration_number",
  "gps_location", 
  "ind_name",
  "health_ind_name", 
  "edu_ind_name", 
  "nut_ind_name", 
  "prot_ind_name", 
  "latitude",
  "longitude",
  "altitude", 
  "deviceid",
  "username",
  "_geopoint_latitude",                                                                          
  "_geopoint_longitude",                                                                         
  "_geopoint_altitude",                                                                          
  "_geopoint_precision",                                                                         
  "pt_sample_lat",                                                                               
  "pt_sample_lon",
  "pt_num_msg",	
  "pt_num_validation_message",
  "pt_num_validation_message_2", 
  "edu_ind_gender_en", 	"edu_ind_note_2"
  
  
)




# ============================================================
# 2. Capture variable names from raw data (what we started with)
# ============================================================
# Note: By this point, some variables have already been removed in previous scripts
# We capture the current state as our "before" to track what will be removed here

# Raw datasets - current state
raw_main_before        <- names(data_hh_main)
raw_health_before      <- names(data_health_ind)
raw_edu_before         <- names(data_edu_ind)
raw_cp_before          <- names(data_child_protection_ind)
raw_roster_before      <- names(data_hh_roster)
raw_prot_before        <- names(data_prot_ind)

# Clean datasets - current state
clean_main_before      <- names(clean_main)
clean_health_before    <- names(clean_health_ind)
clean_edu_before       <- names(clean_edu_ind)
clean_cp_before        <- names(clean_child_protection_ind)
clean_roster_before    <- names(clean_roster)





# ============================================================
# 3. Remove Kobo metadata, calculated fields and PII variables
# ============================================================

# Function to remove PII and unwanted columns
remove_pii_and_metadata <- function(df) {
  df |>
    dplyr::select(
      -any_of(pii_cols),
      -contains("audit_URL"),
      -contains("organisation"),
      -contains("enumerator_id"),
      -contains("gps"),
      -starts_with("check_")
    )
}

# Remove PII from raw datasets
data_hh_main <- remove_pii_and_metadata(data_hh_main)
data_health_ind <- remove_pii_and_metadata(data_health_ind)
data_edu_ind <- remove_pii_and_metadata(data_edu_ind)
data_child_protection_ind <- remove_pii_and_metadata(data_child_protection_ind)
data_hh_roster <- remove_pii_and_metadata(data_hh_roster)
data_prot_ind <- remove_pii_and_metadata(data_prot_ind)

# Remove PII from clean datasets
clean_main <- remove_pii_and_metadata(clean_main)
clean_health_ind <- remove_pii_and_metadata(clean_health_ind)
clean_edu_ind <- remove_pii_and_metadata(clean_edu_ind)
clean_child_protection_ind <- remove_pii_and_metadata(clean_child_protection_ind)
clean_roster <- remove_pii_and_metadata(clean_roster)


# ============================================================
# 4. Capture variable names BEFORE score/cat removal
# ============================================================
# These will be used to track what gets removed in the next section

raw_main_before_score_cat        <- names(data_hh_main)
raw_health_before_score_cat      <- names(data_health_ind)
raw_edu_before_score_cat         <- names(data_edu_ind)
raw_cp_before_score_cat          <- names(data_child_protection_ind)
raw_roster_before_score_cat      <- names(data_hh_roster)
raw_prot_before_score_cat        <- names(data_prot_ind)

clean_main_before_score_cat      <- names(clean_main)
clean_health_before_score_cat    <- names(clean_health_ind)
clean_edu_before_score_cat       <- names(clean_edu_ind)
clean_cp_before_score_cat        <- names(clean_child_protection_ind)
clean_roster_before_score_cat    <- names(clean_roster)


# ============================================================
# 5. Provide rationales for variables added during cleaning
# ============================================================
# Add every newly computed variable and its formula/reason here.

added_variable_rationales <- tibble::tibble(
  variable = character(),
  rationale = character()
)

# Example: If you added new variables during cleaning, add them here:
# added_variable_rationales <- tibble::tibble(
#   variable = c("new_var_1", "new_var_2"),
#   rationale = c("Computed from X and Y", "Standardized variable Z")
# )


# ============================================================
# 6. Function for assigning reasons to removed variables
# ============================================================

get_removal_rationale <- function(variable) {
  
  dplyr::case_when(
    variable %in% pii_cols ~
      "PII",
    
    stringr::str_detect(
      variable,
      stringr::regex("audit_URL", ignore_case = TRUE)
    ) ~
      "Kobo audit metadata URL",
    
    stringr::str_detect(
      variable,
      stringr::regex("organisation", ignore_case = TRUE)
    ) ~
      "Organisation metadata field",
    
    stringr::str_detect(
      variable,
      stringr::regex("enumerator_id", ignore_case = TRUE)
    ) ~
      "Enumerator identifier",
    
    stringr::str_detect(
      variable,
      stringr::regex("gps", ignore_case = TRUE)
    ) ~
      "GPS or location information",
    
    startsWith(variable, "check_") ~
      "Kobo calculated or validation field",
    
    TRUE ~
      "Removed during data cleaning"
  )
}


# ============================================================
# 7. Function to compare variables before and after cleaning
# ============================================================

create_variable_log <- function(before_variables,
                                after_variables,
                                dataset_name) {
  
  removed_variables <- setdiff(
    before_variables,
    after_variables
  )
  
  added_variables <- setdiff(
    after_variables,
    before_variables
  )
  
  removed_log <- tibble::tibble(
    dataset = rep(dataset_name, length(removed_variables)),
    variable = removed_variables,
    action = rep("Removed", length(removed_variables)),
    rationale = get_removal_rationale(removed_variables)
  )
  
  added_log <- tibble::tibble(
    dataset = rep(dataset_name, length(added_variables)),
    variable = added_variables,
    action = rep("Added", length(added_variables))
  ) |>
    left_join(
      added_variable_rationales,
      by = "variable"
    ) |>
    mutate(
      rationale = coalesce(
        rationale,
        "Added during data cleaning; formula or source not yet documented"
      )
    )
  
  bind_rows(
    removed_log,
    added_log
  )
}


# ============================================================
# 8. PLACEHOLDER: Variable tracking log will be created after score/cat removal
# ============================================================
# We'll create the tracker after removing score/cat variables so we can capture all removals


# ============================================================
# 9. Handle the existing cleaning log safely
# ============================================================

if (is.data.frame(cleaning_log) &&
    "change_type" %in% names(cleaning_log)) {
  
  final_cleaning_log_no_dl <- cleaning_log |>
    filter(change_type != "remove_survey")
  
} else {
  
  final_cleaning_log_no_dl <- tibble()
  
  warning(
    "cleaning_log was NULL, was not a data frame, or did not contain change_type."
  )
}


# ============================================================
# 10. Continue tracking newly created objects
# ============================================================

all_dfs_names_after <- Filter(
  function(x) is(x, "data.frame"),
  mget(ls())
) |>
  names()

all_lists_names_after <- Filter(
  function(x) is(x, "list"),
  mget(ls())
) |>
  names()


new_dataframes <- setdiff(
  all_dfs_names_after,
  all_dfs_names_before
)

new_lists <- setdiff(
  all_lists_names_after,
  all_lists_names_before
)


if (length(new_dataframes) > 0) {
  
  cat(blue("The new data frames created are:"))
  cat("\n")
  
  for (df in new_dataframes) {
    cat(df, "\n")
  }
}


if (length(new_lists) > 0) {
  
  cat(blue("The new lists created are:"))
  cat("\n")
  
  for (object_name in new_lists) {
    cat(object_name, "\n")
  }
}






# 1. Track variables ending with "_score" OR "_cat"   

# Packages
library(dplyr)
library(purrr)
library(tibble)
library(openxlsx)

# List of datasets
datasets <- c(
  "data_hh_main",
  "data_hh_roster",
  "data_edu_ind",
  "data_health_ind",
  "data_nutrition_ind",
  "data_child_protection_ind",
  "clean_main",
  "clean_roster",
  "clean_edu_ind",
  "clean_health_ind",
  "clean_nutrition_ind",
  "clean_child_protection_ind"
)


# ---------------------------------------------------------
# 1. Track variables ending with "_score" OR "_cat"
# ---------------------------------------------------------

removed_variables_tracker <- map_dfr(datasets, function(df_name) {

  df <- get(df_name, envir = .GlobalEnv)

  vars_to_remove <- names(df)[grepl("(_score|_cat)$", names(df))]

  tibble(
    dataset = df_name,
    variable = vars_to_remove
  )
})

# ---------------------------------------------------------
# 2. Export tracked variables to Excel
# ---------------------------------------------------------

write.xlsx(
  removed_variables_tracker,
  file = "removed_score_cat_variables.xlsx",
  overwrite = TRUE
)


# ---------------------------------------------------------
# 3. Remove variables ending with "_score" OR "_cat"
# ---------------------------------------------------------

for (df_name in datasets) {

  df <- get(df_name, envir = .GlobalEnv)

  df <- df %>%
    select(-matches("(_score|_cat)$"))

  assign(df_name, df, envir = .GlobalEnv)
}



### working on the Food security variables

# table(clean_main)



clean_main$fsl_hhs_sleephungry[
  clean_main$fsl_hhs_sleephungry == "no" & !is.na(clean_main$fsl_hhs_sleephungry_freq)
] <- "yes"

# Check: the "no" row should now be all zeros
table(clean_main$fsl_hhs_sleephungry, clean_main$fsl_hhs_sleephungry_freq)

# Apply same logic to alldaynight
clean_main$fsl_hhs_alldaynight[
  clean_main$fsl_hhs_alldaynight == "no" & !is.na(clean_main$fsl_hhs_alldaynight_freq)
] <- "yes"

# Check: the "no" row should now have no frequencies
table(clean_main$fsl_hhs_alldaynight, clean_main$fsl_hhs_alldaynight_freq, useNA = "ifany")


clean_main$fsl_hhs_sleephungry[
  clean_main$fsl_hhs_sleephungry == "yes" & is.na(clean_main$fsl_hhs_sleephungry_freq)
] <- "no"


# Check: every "yes" should now have a frequency
table(clean_main$fsl_hhs_sleephungry, clean_main$fsl_hhs_sleephungry_freq, useNA = "ifany")

# Check: every "yes" should now have a frequency
table(clean_main$fsl_hhs_alldaynight, clean_main$fsl_hhs_alldaynight_freq, useNA = "ifany")



clean_main <- clean_main |>
  select(-"fsl_hhs_nofoodhh_recoded",
         -"fsl_hhs_nofoodhh_freq_recoded",
         -"fsl_hhs_sleephungry_recoded",
         -"fsl_hhs_sleephungry_freq_recoded",
         -"fsl_hhs_alldaynight_recoded",
         -"fsl_hhs_alldaynight_freq_recoded",
         -"fsl_hhs_comp1",
         -"fsl_hhs_comp2",
         -"fsl_hhs_comp3",
         -"fsl_hhs_cat_ipc")


clean_main <- add_fcs(clean_main)
clean_main <- add_rcsi(clean_main)
clean_main <- add_hhs(clean_main)
clean_main <- add_lcsi(clean_main)
clean_main <- add_fcm_phase(clean_main)
clean_main <- add_fclcm_phase(clean_main)
clean_main <- add_comp_foodsec(clean_main)


## tracking all the added variables 
# Create tracking table for newly added indicator variables
# new_indicator_variables <- tibble(
#   variable = c(
#     "fsl_hhs_nofoodhh_recoded",
#     "fsl_hhs_nofoodhh_freq_recoded",
#     "fsl_hhs_sleephungry_recoded",
#     "fsl_hhs_sleephungry_freq_recoded",
#     "fsl_hhs_alldaynight_recoded",
#     "fsl_hhs_alldaynight_freq_recoded",
#     "fsl_hhs_comp1",
#     "fsl_hhs_comp2",
#     "fsl_hhs_comp3",
#     "fsl_hhs_score",
#     "fsl_hhs_cat_ipc",
#     "fsl_hhs_cat",
#     "fsl_lcsi_cat",
#     "comp_foodsec_score"
#   ),
#   action = "Added",
#   rationale = c(
#     "HHS recoded component",
#     "HHS frequency recoded component",
#     "HHS sleep hungry recoded component",
#     "HHS sleep hungry frequency recoded component",
#     "HHS all day/night recoded component",
#     "HHS all day/night frequency recoded component",
#     "HHS composite component 1",
#     "HHS composite component 2",
#     "HHS composite component 3",
#     "Household Hunger Scale score",
#     "HHS IPC category",
#     "HHS category",
#     "Livelihood Coping Strategies Index category",
#     "Composite Food Security Score"
#   )
# )



# Display the tracking table
cat("\nVariables Added During Indicator Processing:\n")
# print(new_indicator_variables)


# # Add to var_tracker
# var_tracker <<- bind_rows(variable_tracker, new_indicator_variables)














# ---------------------------------------------------------
# 4. Capture variable names AFTER score/cat removal
# ---------------------------------------------------------

raw_main_after        <- names(data_hh_main)
raw_health_after      <- names(data_health_ind)
raw_edu_after         <- names(data_edu_ind)
raw_cp_after          <- names(data_child_protection_ind)
raw_roster_after      <- names(data_hh_roster)
raw_prot_after        <- names(data_prot_ind)

clean_main_after      <- names(clean_main)
clean_health_after    <- names(clean_health_ind)
clean_edu_after       <- names(clean_edu_ind)
clean_cp_after        <- names(clean_child_protection_ind)
clean_roster_after    <- names(clean_roster)






# ---------------------------------------------------------
# 5. Generate the variable tracking log (AFTER all removals)
# ---------------------------------------------------------

variable_tracker_detailed <- bind_rows(
  
  # Raw datasets
  create_variable_log(
    before_variables = raw_main_before,
    after_variables = raw_main_after,
    dataset_name = "data_hh_main (raw)"
  ),
  
  create_variable_log(
    before_variables = raw_health_before,
    after_variables = raw_health_after,
    dataset_name = "data_health_ind (raw)"
  ),
  
  create_variable_log(
    before_variables = raw_edu_before,
    after_variables = raw_edu_after,
    dataset_name = "data_edu_ind (raw)"
  ),
  
  create_variable_log(
    before_variables = raw_cp_before,
    after_variables = raw_cp_after,
    dataset_name = "data_child_protection_ind (raw)"
  ),
  
  create_variable_log(
    before_variables = raw_roster_before,
    after_variables = raw_roster_after,
    dataset_name = "data_hh_roster (raw)"
  ),
  
  create_variable_log(
    before_variables = raw_prot_before,
    after_variables = raw_prot_after,
    dataset_name = "data_prot_ind (raw)"
  ),
  
  # Clean datasets
  create_variable_log(
    before_variables = clean_main_before,
    after_variables = clean_main_after,
    dataset_name = "clean_main"
  ),
  
  create_variable_log(
    before_variables = clean_health_before,
    after_variables = clean_health_after,
    dataset_name = "clean_health_ind"
  ),
  
  create_variable_log(
    before_variables = clean_edu_before,
    after_variables = clean_edu_after,
    dataset_name = "clean_edu_ind"
  ),
  
  create_variable_log(
    before_variables = clean_cp_before,
    after_variables = clean_cp_after,
    dataset_name = "clean_child_protection_ind"
  ),
  
  create_variable_log(
    before_variables = clean_roster_before,
    after_variables = clean_roster_after,
    dataset_name = "clean_roster"
  )
)

# Requested three-column version
variable_tracker <- variable_tracker_detailed |>
  select(
    variable,
    action,
    rationale
  ) |>
  distinct() |>
  arrange(
    factor(action, levels = c("Removed", "Added")),
    variable
  )

# Display the variable log
print(variable_tracker)


# # Add to var_tracker
# var_tracker <<- bind_rows(variable_tracker, new_indicator_variables)

# ---------------------------------------------------------
# 6. Review variables that were removed
# ---------------------------------------------------------

# print(removed_variables_tracker)

# 
# table(clean_health_ind$health_ind_healthcare_needed_cat)


# table(clean_main$weight)




cat("\n")
cat(green$bold("Obsolete variables removed and variable log created"))
cat("\n\n")

beepr::beep()