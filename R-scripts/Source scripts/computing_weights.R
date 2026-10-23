# =============================================================================
# MOZ2603 – Mozambique MSNA 2026 – Sampling weights
# =============================================================================
# Strata (ToR):  refugees       -> settlement (14)
#                host community -> district   (12)
#                Kampala included (both refugee and host community)
#
# Weight per stratum h:
#     weight_h = (N_h / N) / (n_h / n)
#   N_h = stratum population, N = total population of all strata
#   n_h = interviews achieved in stratum, n = total interviews
#
# - Weights sum to the number of interviews (mean weight = 1).
# - Weight is constant inside a stratum -> settlement / district results are
#   identical weighted or unweighted (as the ToR requires).
# - The same weight is valid for any aggregation (all settlements, region,
#   all host districts, refugee + host).
# - Individual loops inherit the household weight (all members interviewed).
# =============================================================================

library(dplyr)
library(stringr)
library(tibble)

table(clean_main$admin2)

# Population frame
# Refugees: UNHCR active population by settlement (ToR Table 1, April 2026)
# Host:     district population (ToR Table 2)
# >>> Replace with the latest UNHCR/OPM figures if they were updated <<<

pop_refugee <- tribble(
  ~strata_name,  ~population,
  "adjumani",       236453,
  "bidibidi",       210353,
  "imvepi",          83698,
  "kiryandongo",    168067,
  "kyaka_ii",       139142,
  "kyangwali",      158546,
  "lobule",           6540,
  "nakivale",       286062,
  "oruchinga",        8258,
  "palabek",        101164,
  "palorinya",      144806,
  "rhino_camp",     206165,
  "rwamwanja",      109369,
  "kampala",        173074
) |> mutate(hoh_type = "refugee")

pop_host <- tribble(
  ~strata_name,  ~population,
  "adjumani",       242700,
  "madi_okollo",    189300,
  "obongi",          55300,
  "terego",         269100,
  "yumbe",          899400,
  "koboko",         318300,
  "lamwo",          151500,
  "isingiro",       721500,
  "kamwenge",       410200,
  "kyegegwa",       686000,
  "kiryandongo",    364500,
  "kampala",        1844500 
) |> mutate(hoh_type = "host_community")

pop_frame <- bind_rows(pop_refugee, pop_host) |>
  mutate(strata = paste(hoh_type, strata_name, sep = "_"))

# Strata variable
# Settlement codes in the tool (l_cluster_id) -> ToR settlement names.
# Adjumani is split into 17 sub-settlements (adjumani_agojo, ...) -> merged.

settlement_lookup <- c(
  isingiro_nakivale       = "nakivale",
  isingiro_oruchinga      = "oruchinga",
  kamwenge_rwamwanja      = "rwamwanja",
  kikuube_kyangwali       = "kyangwali",
  kiryandongo_kiryandongo = "kiryandongo",
  koboko_lobule           = "lobule",
  kyegegwa_kyaka_ii       = "kyaka_ii",
  lamwo_palabek           = "palabek",
  madi_okollo_rhino       = "rhino_camp",
  obongi_palorinya        = "palorinya",
  terego_imvepi           = "imvepi",
  yumbe_bidibidi          = "bidibidi"
)

add_strata <- function(df) {
  df |>
    mutate(
      hoh_type         = as.character(hoh_type),
      admin2           = as.character(admin2),
      admin3_setlement = as.character(admin3_setlement),
      strata_name = case_when(
        # Refugee: extract settlement name from admin3_setlement
        # admin3_setlement format is "district_settlement" or "district_zone"
        # We need just the settlement name part based on mapping
        hoh_type == "refugee" & admin2 == "adjumani" ~ "adjumani",
        hoh_type == "refugee" & admin2 == "bidibidi" ~ "bidibidi",
        hoh_type == "refugee" & admin2 == "imvepi" ~ "imvepi",
        hoh_type == "refugee" & admin2 == "kiryandongo" ~ "kiryandongo",
        hoh_type == "refugee" & admin2 == "kyegegwa" ~ "kyaka_ii",
        hoh_type == "refugee" & admin2 == "kyangwali" ~ "kyangwali",
        hoh_type == "refugee" & admin2 == "koboko" ~ "lobule",
        hoh_type == "refugee" & admin2 == "isingiro" & stringr::str_detect(admin3_setlement, "nakivale") ~ "nakivale",
        hoh_type == "refugee" & admin2 == "isingiro" & stringr::str_detect(admin3_setlement, "oruchinga") ~ "oruchinga",
        hoh_type == "refugee" & admin2 == "lamwo" ~ "palabek",
        hoh_type == "refugee" & admin2 == "madi" ~ "rhino_camp",
        hoh_type == "refugee" & admin2 == "madi_okollo" ~ "rhino_camp",
        hoh_type == "refugee" & admin2 == "obongi" ~ "palorinya",
        hoh_type == "refugee" & admin2 == "terego" ~ "imvepi",
        hoh_type == "refugee" & admin2 == "kamwenge" ~ "rwamwanja",
        hoh_type == "refugee" & admin2 == "kikuube" ~ "kyangwali",
        hoh_type == "refugee" & admin2 == "yumbe" ~ "bidibidi",
        hoh_type == "refugee" & admin2 == "kampala" ~ "kampala",
        # Host community: use admin2, but normalize "madi" to "madi_okollo"
        hoh_type == "host_community" & admin2 == "madi" ~ "madi_okollo",
        hoh_type == "host_community" ~ admin2,
        TRUE ~ NA_character_
      ),
      strata = if_else(is.na(strata_name), NA_character_,
                       paste(hoh_type, strata_name, sep = "_"))
    )
}

# Household dataset: build strata, validate
# Note: Remove old weight column if present in clean_main to avoid name collision
main_w <- clean_main |>
  select(-any_of("weight")) |>
  add_strata()

# Every interview must fall in a stratum of the population frame.
unmatched <- main_w |> filter(is.na(strata) | !strata %in% pop_frame$strata)
if (nrow(unmatched) > 0) {
  cat("\n❌ ERROR: The following interviews do not match a stratum:\n")
  print(unmatched |> select(hoh_type, admin2, admin3_setlement, strata))
  stop(nrow(unmatched), " interviews do not match a stratum - fix or remove them before weighting.")
}

# Weights
weights_tbl <- main_w |>
  count(strata, name = "n_achieved") |>
  inner_join(pop_frame, by = "strata") |>
  mutate(
    pop_share    = population / sum(population),
    sample_share = n_achieved / sum(n_achieved),
    weight       = pop_share / sample_share
  ) |>
  select(hoh_type, strata_name, strata, population, n_achieved,
         pop_share, sample_share, weight) |>
  arrange(hoh_type, strata_name)

# Strata in the frame with no interviews -> they cannot be represented
missing_strata <- anti_join(pop_frame, weights_tbl, by = "strata")
if (nrow(missing_strata) > 0) {
  warning("No interviews in: ", paste(missing_strata$strata, collapse = ", "))
}

main_w <- main_w |>
  left_join(select(weights_tbl, strata, weight), by = "strata")

# Checks
stopifnot(
  !anyNA(main_w$weight),
  all(main_w$weight > 0),
  isTRUE(all.equal(sum(main_w$weight), nrow(main_w)))   # weights sum to n
)


# Weighted share of each stratum must equal its population share
check_shares <- main_w |>
  group_by(strata) |>
  summarise(weighted_share = sum(weight) / sum(main_w$weight), .groups = "drop") |>
  left_join(select(weights_tbl, strata, pop_share), by = "strata")
stopifnot(isTRUE(all.equal(check_shares$weighted_share, check_shares$pop_share)))

print(weights_tbl, n = Inf)

# Individual-level loops: same strata, household weight
# Join strata + weight from household data by household id
# Loop _submission__uuid = main _uuid

# Roster
clean_roster_w <- clean_roster |>
  inner_join(
    main_w |> select(`_uuid`, strata, weight),
    by = c("_submission__uuid" = "_uuid")
  )
cat("\nclean_roster_w: ", nrow(clean_roster_w), " rows retained from ", nrow(clean_roster), "\n")

# Education
clean_edu_ind_w <- clean_edu_ind |>
  inner_join(
    main_w |> select(`_uuid`, strata, weight),
    by = c("_submission__uuid" = "_uuid")
  )
cat("clean_edu_ind_w: ", nrow(clean_edu_ind_w), " rows retained from ", nrow(clean_edu_ind), "\n")



# Health
clean_health_ind_w <- clean_health_ind |>
  inner_join(
    main_w |> select(`_uuid`, strata, weight),
    by = c("_submission__uuid" = "_uuid")
  )
cat("clean_health_ind_w: ", nrow(clean_health_ind_w), " rows retained from ", nrow(clean_health_ind), "\n")

# Nutrition
clean_nutrition_ind_w <- clean_nutrition_ind |>
  inner_join(
    main_w |> select(`_uuid`, strata, weight),
    by = c("_submission__uuid" = "_uuid")
  )
cat("clean_nutrition_ind_w: ", nrow(clean_nutrition_ind_w), " rows retained from ", nrow(clean_nutrition_ind), "\n")

# Child protection
clean_child_protection_w <- clean_child_protection_ind |>
  inner_join(
    main_w |> select(`_uuid`, strata, weight),
    by = c("_submission__uuid" = "_uuid")
  )
cat("clean_child_protection_w: ", nrow(clean_child_protection_w), " rows retained from ", nrow(clean_child_protection_ind), "\n")

# Checks: no NA weights in any loop
stopifnot(!anyNA(clean_roster_w$weight))
stopifnot(!anyNA(clean_edu_ind_w$weight))
stopifnot(!anyNA(clean_health_ind_w$weight))
stopifnot(!anyNA(clean_nutrition_ind_w$weight))
stopifnot(!anyNA(clean_child_protection_w$weight))

cat("\n✅ All weight checks passed!\n")


# # renaming the datasets to 
# , , , , clean_child_protection_ind


clean_main<-main_w

clean_roster<-clean_roster_w

clean_edu_ind<-clean_edu_ind_w

clean_health_ind<-clean_health_ind_w

clean_nutrition_ind<-clean_nutrition_ind_w 

clean_child_protection_ind<-clean_child_protection_w



# Save
write.csv(weights_tbl, "MOZ2603_MSNU_2026_weights_kampala.csv", row.names = FALSE)

# Count of interviews by hoh_type x strata
cat("\n\nCount of interviews by hoh_type x strata:\n")
interview_count <- main_w |>
  group_by(hoh_type, strata_name) |>
  summarise(n_interviews = n(), .groups = "drop") |>
  arrange(hoh_type, strata_name)
print(interview_count, n = Inf)

# Using the weights (srvyr)
# library(srvyr)
#
# Household level
# design_hh <- main_w |>
#   as_survey_design(strata = strata, weights = weight)
#   # Host community clusters: if you have the hexagon / cluster ID, use
#   # as_survey_design(ids = cluster_id, strata = strata, weights = weight, nest = TRUE)
#
# Individual level: members are clustered within households
# design_ind <- clean_roster_w |>
#   as_survey_design(ids = `_submission__uuid`, strata = strata, weights = weight)
#
# Example: % households by drinking water source, all refugee settlements combined
# design_hh |>
#   filter(hoh_type == "refugee") |>
#   group_by(wash_drinking_water_source) |>
#   summarise(pct = survey_prop(vartype = "ci"))
