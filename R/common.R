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
property_profile <- read_rds("data/output/property_profile.rds")
# Load geocoded data if available
if(file.exists("data/output/geocoded_properties.csv")) {
  geocoded <- read_csv("data/output/geocoded_properties.csv", show_col_types = FALSE) %>%
    mutate(uid = as.character(uid))
  
  property_profile <- property_profile %>%
    mutate(uid = as.character(uid)) %>%
    left_join(geocoded, by = "uid", suffix = c("", "_geo")) %>%
    mutate(
      lat = coalesce(lat, geocoded_lat),
      lon = coalesce(lon, geocoded_lon)
    ) %>%
    select(-ends_with("_geo"))
}
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
    
    # Land use flags
    use_church = !is.na(church) & church == 1,
    use_cemetery = !is.na(cemetery) & cemetery == 1,
    use_school = !is.na(school) & school == 1,
    use_parking = !is.na(parking) & parking == 1,
    use_open_space = !is.na(open_space) & open_space == 1,
    use_residence = !is.na(residence) & residence == 1,
    
    # ========================================
    # COMPONENT SCORES (0-100)
    # ========================================
    
    # 1. SIZE SCORE (Weight: 20%)
    size_score = case_when(
      area_acres < 0.25 ~ 20,
      area_acres < 0.5 ~ 50,
      area_acres < 1 ~ 70,
      area_acres < 2 ~ 90,
      area_acres < 5 ~ 100,
      area_acres < 10 ~ 85,
      TRUE ~ 70
    ),
    
    # 2. USE SCORE (Weight: 25%)
    use_score = case_when(
      use_parking ~ 95,
      use_open_space ~ 90,
      use_cemetery ~ 5,
      use_church & !use_parking & !use_open_space ~ 30,
      use_residence ~ 40,
      use_school ~ 35,
      TRUE ~ 60
    ),
    
    # 3. LOCATION SCORE (Weight: 20%)
    walkability_score = replace_na(walk_idx, 0),
    location_score = pmin(100, pmax(0, walkability_score * 5)),  # Scale to 0-100
    
    # 4. FINANCIAL NEED SCORE (Weight: 15%)
    financial_score = case_when(
      !is.na(pct_change_pledge) & pct_change_pledge < -20 ~ 100,
      !is.na(pct_change_pledge) & pct_change_pledge < 0 ~ 70,
      TRUE ~ 30
    ),
    
    financial_need = case_when(
      !is.na(pct_change_pledge) & pct_change_pledge < -20 ~ 3,
      !is.na(pct_change_pledge) & pct_change_pledge < 0 ~ 2,
      TRUE ~ 1
    ),
    
    # 5. MARKET SCORE (Weight: 10%)
    market_score = case_when(
      qct == 1 ~ 90,
      dda == 1 ~ 70,
      TRUE ~ 50
    ),
    
    # 6. ZONING SCORE (Weight: 10%)
    zoning_score = 50,  # Placeholder until zoning analysis
    
    # ========================================
    # CONSTRAINT PENALTIES
    # ========================================
    constraint_penalty = case_when(
      has_environmental_constraint & wet_perc > 25 ~ -75,  # Both flood + major wetlands
      has_environmental_constraint & !is.na(wet_perc) & wet_perc > 10 ~ -40,  # Flood or moderate wetlands
      has_environmental_constraint ~ -20,  # Minor constraints
      TRUE ~ 0
    ),
    
    # ========================================
    # WEIGHTED DEVELOPMENT SCORE
    # ========================================
    development_score_raw = (
      (size_score * 0.20) +
        (use_score * 0.25) +
        (location_score * 0.20) +
        (financial_score * 0.15) +
        (market_score * 0.10) +
        (zoning_score * 0.10)
    ),
    
    development_score = pmax(0, pmin(100, development_score_raw + constraint_penalty)),
    
    # ========================================
    # TIER CLASSIFICATION (Based on score)
    # ========================================
    development_tier = case_when(
      development_score >= 75 ~ "Tier 1: High Priority",
      development_score >= 60 ~ "Tier 2: Strong Potential",
      development_score >= 45 ~ "Tier 3: Moderate Potential",
      development_score >= 30 ~ "Tier 4: Limited Potential",
      TRUE ~ "Tier 5: Not Recommended"
    ),
    
    # Defaults/placeholders
    bldgcount = 1,
    year_built = 1950,
    has_congregation = !is.na(congregation_name)
  )

# ========================================
# ADD CATEGORICAL CLASSIFICATION IF MISSING
# ========================================

# Check if development_potential exists, if not, create it
if(!"development_potential" %in% names(property_profile)) {
  property_profile <- property_profile %>%
    mutate(
      # Goldilocks categorical classification
      development_potential = case_when(
        # Goldilocks range (0.5-5 acres)
        area_acres >= 1.5 & area_acres <= 5 & !has_environmental_constraint ~ "High",
        area_acres >= 0.5 & area_acres < 1.5 & !has_environmental_constraint ~ "Moderate",
        
        # Outside goldilocks
        area_acres > 5 ~ "Too Large",
        area_acres < 0.5 ~ "Small Parcel",
        has_environmental_constraint ~ "Constrained",
        TRUE ~ "Review Needed"
      )
    )
}

# ========================================
# CREATE FILTERED SUBSETS
# ========================================

# Scored parcels: for mapping (all with coordinates)
scored_parcels <- property_profile %>%
  filter(!is.na(area_acres), area_acres > 0.1, !is.na(lat) & !is.na(lon))

# Top Parcels: Goldilocks range (0.5-5 acres) with High/Moderate potential
top_parcels <- property_profile %>%
  filter(
    development_potential %in% c("High", "Moderate"),
    area_acres >= 0.5 & area_acres <= 5
  ) %>%
  arrange(desc(development_score)) %>%
  mutate(rank = row_number())

# Save enhanced property profile
saveRDS(property_profile, "data/output/property_profile_scored.rds")
write_csv(property_profile, "data/output/property_profile_scored.csv")

# Export top opportunities (goldilocks only)
write_csv(top_parcels, "data/output/verep_top_opportunities.csv")