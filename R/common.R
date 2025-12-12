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

# ========================================
# LOAD DATA
# ========================================
property_profile <- read_rds("data/output/property_profile_improved.rds")
attendance_context <- read_csv("data/output/verep_attendance_summary.csv", show_col_types = FALSE)
opportunity_matrix <- read_csv("data/output/verep_opportunity_matrix.csv", show_col_types = FALSE)
leadership_report <- read_csv("data/output/verep_top_opportunities.csv", show_col_types = FALSE)
data_followup <- read_csv("data/output/verep_data_needed.csv", show_col_types = FALSE)

no_congregation <- read_csv("data/output/unmatched_no_congregation.csv", show_col_types = FALSE)
no_match <- read_csv("data/output/unmatched_name_not_found.csv", show_col_types = FALSE)
name_exists_city_different <- read_csv("data/output/unmatched_city_mismatch.csv", show_col_types = FALSE)
name_not_found <- read_csv("data/output/unmatched_name_missing.csv", show_col_types = FALSE)

# ========================================
# ADD CALCULATED FIELDS TO PROPERTY_PROFILE
# ========================================
property_profile <- property_profile %>%
  mutate(
    # Aliases
    area_acres = rgisacre,
    assessed_value = lan_val,
    site_address = sadd,
    site_city = scity,
    site_county = scounty,
    name = congregation_name,
    avg_attendance = attendance_2023,
    avg_pledge = plate_pledge_2023,
    avg_members = members_2023,
    
    # Development score
    development_score = case_when(
      development_potential == "High" ~ 85,
      development_potential == "Medium" ~ 70,
      development_potential == "Constrained" ~ 40,
      development_potential == "Small Parcel" ~ 30,
      development_potential == "Review Needed" ~ 20,
      TRUE ~ 20
    ),
    development_tier = development_potential,
    
    # Land use flags
    use_church = !is.na(church) & church == 1,
    use_cemetery = !is.na(cemetery) & cemetery == 1,
    use_school = !is.na(school) & school == 1,
    use_parking = !is.na(parking) & parking == 1,
    use_open_space = !is.na(open_space) & open_space == 1,
    use_residence = !is.na(residence) & residence == 1,
    
    # Scores
    walkability_score = replace_na(walk_idx, 0),
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
    market_score = 50,
    zoning_score = 50,
    
    # Financial need tier
    financial_need = case_when(
      !is.na(pct_change_pledge) & pct_change_pledge < -20 ~ 3,
      !is.na(pct_change_pledge) & pct_change_pledge < 0 ~ 2,
      TRUE ~ 1
    ),
    
    # Defaults/placeholders
    bldgcount = 1,
    year_built = 1950,
    has_congregation = !is.na(congregation_name)
  )

# ========================================
# CREATE FILTERED SUBSETS
# ========================================

# Scored parcels: filtered for mapping (has coordinates and minimum size)
scored_parcels <- property_profile %>%
  filter(!is.na(area_acres), area_acres > 0.1, !is.na(lat) & !is.na(lon))

# Top Parcels: highest value High/Medium potential properties
top_parcels <- scored_parcels %>%
  filter(development_potential %in% c("High", "Medium")) %>%
  arrange(desc(assessed_value)) %>%
  slice_head(n = 10) %>%
  mutate(rank = row_number())