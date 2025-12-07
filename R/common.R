# _common.R
library(tidyverse)
library(shiny)
library(bslib)
library(DT)
library(ggplot2)
library(plotly)
library(rsconnect)
library(kableExtra)
library(leaflet)
library(leaflet.extras)


attendance_context <- read_csv("data/output/verep_attendance_summary.csv", show_col_types = FALSE)
opportunity_matrix <- read_csv("data/output/verep_opportunity_matrix.csv", show_col_types = FALSE)
leadership_report <- read_csv("data/output/verep_top_opportunities.csv", show_col_types = FALSE)
data_followup <- read_csv("data/output/verep_data_needed.csv", show_col_types = FALSE)

property_profile <- read_rds("data/output/property_profile.rds")
no_congregation <- read_csv("data/output/unmatched_no_congregation.csv") # No congregation assigned
no_match <- read_csv("data/output/unmatched_name_not_found.csv") # Has name but didn't match
name_exists_city_different <- read_csv("data/output/unmatched_city_mismatch.csv")
name_not_found <- read_csv("data/output/unmatched_name_missing.csv") #Name not in congregation data

# Create scored_parcels with development scores
scored_parcels <- property_profile %>%
  mutate(
    area_acres = rgisacre,
    development_score = case_when(
      development_potential == "High Potential" ~ 85,
      development_potential == "Moderate Potential" ~ 70,
      development_potential == "Low Potential - Constraints" ~ 40,
      development_potential == "Insufficient Data" ~ 30,
      TRUE ~ 20
    ),
    development_tier = development_potential,
    assessed_value = lan_val,
    use_church = !is.na(church) & church == 1,
    use_cemetery = !is.na(cemetery) & cemetery == 1,
    use_school = !is.na(school) & school == 1,
    use_parking = !is.na(parking) & parking == 1,
    use_open_space = !is.na(open_space) & open_space == 1,
    use_residence = !is.na(residence) & residence == 1,
    walkability_score = replace_na(walk_idx, 0),
    site_address = sadd,
    site_city = scity,
    site_county = scounty,
    # Use existing scores from preprocessed file or set defaults
    size_score = case_when(
      area_acres < 0.25 ~ 20, area_acres < 0.5 ~ 50, area_acres < 1 ~ 70,
      area_acres < 2 ~ 90, area_acres < 5 ~ 100, area_acres < 10 ~ 85,
      TRUE ~ 70
    ),
    use_score = case_when(
      use_parking ~ 95, use_open_space ~ 90, use_cemetery ~ 5,
      use_church & !use_parking & !use_open_space ~ 30,
      use_residence ~ 40, use_school ~ 35, TRUE ~ 60
    ),
    location_score = pmin(100, pmax(0, walkability_score)),
    financial_score = case_when(
      !is.na(pct_change_pledge) & pct_change_pledge < -20 ~ 100,
      !is.na(pct_change_pledge) & pct_change_pledge < 0 ~ 70,
      TRUE ~ 30
    ),
    market_score = 50,  # Default
    zoning_score = 50,  # Default
    # Congregation data
    avg_attendance = attendance_2023,
    avg_members = 0,
    avg_pledge = plate_pledge_2023,
    financial_need = case_when(
      !is.na(pct_change_pledge) & pct_change_pledge < -20 ~ 3,
      !is.na(pct_change_pledge) & pct_change_pledge < 0 ~ 2,
      TRUE ~ 1
    ),
    name = congregation_name
  ) %>%
  filter(!is.na(area_acres), area_acres > 0.1, !is.na(lat) & !is.na(lon))

# Create top_10
top_10 <- scored_parcels %>%
  filter(development_potential %in% c("High Potential", "Moderate Potential")) %>%
  arrange(desc(assessed_value)) %>%
  slice_head(n = 10) %>%
  mutate(rank = row_number())

# Create enhanced version with aliases and calculated fields
property_profile <- property_profile |>
  mutate(
    area_acres = rgisacre,
    development_score = case_when(
      development_potential == "High Potential" ~ 85,
      development_potential == "Moderate Potential" ~ 70,
      development_potential == "Low Potential - Constraints" ~ 40,
      development_potential == "Insufficient Data" ~ 30,
      TRUE ~ 20
    ),
    development_tier = development_potential,
    assessed_value = lan_val,
    use_church = !is.na(church) & church == 1,
    use_parking = !is.na(parking) & parking == 1,
    use_open_space = !is.na(open_space) & open_space == 1,
    use_cemetery = !is.na(cemetery) & cemetery == 1,
    use_school = !is.na(school) & school == 1,
    use_residence = !is.na(residence) & residence == 1,
    avg_attendance = attendance_2023,
    avg_pledge = plate_pledge_2023,
    bldgcount = 1,
    year_built = 1950,
    has_congregation = !is.na(congregation_name)
  )
