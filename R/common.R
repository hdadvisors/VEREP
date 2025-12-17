# ========================================
# COMMON.R - SCORING AND ANALYSIS
# ========================================

library(tidyverse)
library(shiny)
library(bslib)
library(DT)
library(ggplot2)
library(plotly)
library(kableExtra)
library(leaflet)
library(leaflet.extras)

# ========================================
# LOAD CLEAN DATA
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

# ========================================
# CALCULATE COMPONENT SCORES (0-100)
# ========================================

property_profile <- property_profile %>%
  mutate(
    # Aliases for compatibility
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
    # 1. SIZE SCORE (Weight: 20%)
    # ========================================
    size_score = case_when(
      area_acres < 0.25 ~ 20,
      area_acres < 0.5 ~ 50,
      area_acres < 1 ~ 70,
      area_acres < 2 ~ 90,
      area_acres < 5 ~ 100,
      area_acres < 10 ~ 85,
      TRUE ~ 70
    ),
    
    # ========================================
    # 2. CURRENT USE SCORE (Weight: 25%)
    # ========================================
    use_score = case_when(
      use_parking ~ 95,
      use_open_space ~ 90,
      use_cemetery ~ 5,
      use_church & !use_parking & !use_open_space ~ 30,
      use_residence ~ 40,
      use_school ~ 35,
      TRUE ~ 60
    ),
    
    # ========================================
    # 3. LOCATION SCORE (Weight: 20%)
    # ========================================
    walkability_score = replace_na(walk_idx, 0),
    location_score = pmin(100, pmax(0, walkability_score * 5)),
    
    # ========================================
    # 4. FINANCIAL NEED SCORE (Weight: 15%)
    # ========================================
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
    
    # ========================================
    # 5. ENHANCED MARKET SCORE (Weight: 10%)
    # Uses median income + income growth
    # ========================================
    market_score = case_when(
      # Strong income + strong growth = premium market
      !is.na(medinc) & !is.na(hhi_g_n5) & 
        medinc >= 75000 & hhi_g_n5 >= 2 ~ 95,
      
      # Good income + growth
      !is.na(medinc) & !is.na(hhi_g_n5) & 
        medinc >= 60000 & hhi_g_n5 >= 1 ~ 85,
      
      # High income area (stable)
      !is.na(medinc) & medinc >= 75000 ~ 80,
      
      # Middle income + positive growth
      !is.na(medinc) & !is.na(hhi_g_n5) & 
        medinc >= 50000 & hhi_g_n5 >= 0 ~ 70,
      
      # Tax credit eligible (DDA/QOZ) - mission alignment
      dda == 1 | (!is.na(qoz) & qoz == 1) ~ 65,
      
      # Moderate income
      !is.na(medinc) & medinc >= 40000 ~ 55,
      
      # Lower income, declining
      !is.na(medinc) & !is.na(hhi_g_n5) & 
        medinc < 40000 & hhi_g_n5 < 0 ~ 35,
      
      # Default for missing data
      TRUE ~ 50
    ),
    
    # ========================================
    # 6. ZONING SCORE (Weight: 10%)
    # ========================================
    zoning_score = 50,  # Placeholder until detailed zoning analysis
    
    # ========================================
    # WEIGHTED DEVELOPMENT SCORE (Before Penalties)
    # ========================================
    development_score_raw = (
      (size_score * 0.20) +
        (use_score * 0.25) +
        (location_score * 0.20) +
        (financial_score * 0.15) +
        (market_score * 0.10) +
        (zoning_score * 0.10)
    ),
    
    # ========================================
    # APPLY CONSTRAINT PENALTIES
    # constraint_penalty comes from data_creation.R
    # ========================================
    development_score = pmax(0, pmin(100, development_score_raw + constraint_penalty)),
    
    # ========================================
    # TIER CLASSIFICATION (Based on final score)
    # ========================================
    development_tier = case_when(
      development_score >= 75 ~ "Tier 1: High Priority",
      development_score >= 60 ~ "Tier 2: Strong Potential",
      development_score >= 45 ~ "Tier 3: Moderate Potential",
      development_score >= 30 ~ "Tier 4: Limited Potential",
      TRUE ~ "Tier 5: Not Recommended"
    ),
    
    # ========================================
    # CONSTRAINT SUMMARY (for display)
    # ========================================
    constraint_summary = case_when(
      has_historic_constraint & has_high_hazard_risk ~ "Historic + High Hazard",
      has_historic_constraint & has_moderate_hazard_risk ~ "Historic + Moderate Hazard",
      has_historic_constraint ~ "Historic District",
      has_high_hazard_risk ~ "High Hazard Risk",
      has_moderate_hazard_risk ~ "Moderate Hazard Risk",
      has_wetland_constraint ~ "Wetlands",
      has_flood_constraint ~ "Flood Zone",
      TRUE ~ "None"
    ),
    
    # Defaults/placeholders
    bldgcount = 1,
    year_built = 1950,
    has_congregation = !is.na(congregation_name)
  )

# ========================================
# CREATE FILTERED SUBSETS
# ========================================

# Scored parcels: for mapping (all with coordinates)
scored_parcels <- property_profile %>%
  filter(!is.na(area_acres), area_acres > 0.1, !is.na(lat) & !is.na(lon))

# Top Parcels: Goldilocks range (0.5-5 acres) with High/Moderate potential
# Sorted by development score (includes constraint penalties)
top_parcels <- property_profile %>%
  filter(
    development_potential %in% c("High", "Moderate"),
    area_acres >= 0.5 & area_acres <= 5
  ) %>%
  arrange(desc(development_score)) %>%
  mutate(rank = row_number())

# ========================================
# LOAD OTHER OUTPUT FILES
# ========================================

if(file.exists("data/output/verep_attendance_summary.csv")) {
  attendance_context <- read_csv("data/output/verep_attendance_summary.csv", show_col_types = FALSE)
}

if(file.exists("data/output/verep_opportunity_matrix.csv")) {
  opportunity_matrix <- read_csv("data/output/verep_opportunity_matrix.csv", show_col_types = FALSE)
}

if(file.exists("data/output/verep_data_needed.csv")) {
  data_followup <- read_csv("data/output/verep_data_needed.csv", show_col_types = FALSE)
}

# Unmatched files
if(file.exists("data/output/unmatched_no_congregation.csv")) {
  no_congregation <- read_csv("data/output/unmatched_no_congregation.csv", show_col_types = FALSE)
}

if(file.exists("data/output/unmatched_name_not_found.csv")) {
  no_match <- read_csv("data/output/unmatched_name_not_found.csv", show_col_types = FALSE)
}

if(file.exists("data/output/unmatched_city_mismatch.csv")) {
  name_exists_city_different <- read_csv("data/output/unmatched_city_mismatch.csv", show_col_types = FALSE)
}

if(file.exists("data/output/unmatched_name_missing.csv")) {
  name_not_found <- read_csv("data/output/unmatched_name_missing.csv", show_col_types = FALSE)
}

# Save enhanced property profile
saveRDS(property_profile, "data/output/property_profile_scored.rds")
write_csv(property_profile, "data/output/property_profile_scored.csv")

# Export top opportunities (goldilocks only)
write_csv(top_parcels, "data/output/verep_top_opportunities.csv")

# ========================================
# SUMMARY STATISTICS
# ========================================

cat("✓ Scoring complete\n\n")

cat("=== TIER DISTRIBUTION ===\n")
property_profile %>%
  count(development_tier) %>%
  arrange(desc(n)) %>%
  print()

cat("\n=== GOLDILOCKS OPPORTUNITIES ===\n")
cat(sprintf("Total High + Moderate: %d (%.1f%%)\n",
            nrow(top_parcels), nrow(top_parcels) / nrow(property_profile) * 100))

cat("\n=== TOP 15 PROPERTIES BY SCORE ===\n")
top_parcels %>%
  slice_head(n = 15) %>%
  select(rank, congregation_name, scity, area_acres, 
         development_score, constraint_summary, development_potential) %>%
  print()

cat("\n=== CONSTRAINT IMPACT ===\n")
constraint_impact <- property_profile %>%
  filter(development_potential %in% c("High", "Moderate")) %>%
  summarise(
    total = n(),
    with_constraints = sum(constraint_penalty < 0),
    avg_score_unconstrained = mean(development_score[constraint_penalty == 0], na.rm = TRUE),
    avg_score_constrained = mean(development_score[constraint_penalty < 0], na.rm = TRUE),
    score_difference = avg_score_unconstrained - avg_score_constrained
  )

print(constraint_impact)

cat("\n=== CONSTRAINT TYPES IN GOLDILOCKS ===\n")
property_profile %>%
  filter(development_potential %in% c("High", "Moderate")) %>%
  count(constraint_summary) %>%
  arrange(desc(n)) %>%
  print()

# ========================================
# SAVE FILES
# ========================================

# Save enhanced property profile
saveRDS(property_profile, "data/output/property_profile_scored.rds")
write_csv(property_profile, "data/output/property_profile_scored.csv")

# Export top opportunities (goldilocks only)
write_csv(top_parcels, "data/output/verep_top_opportunities.csv")

cat("\n✓ Files saved:\n")
cat("  - property_profile_scored.rds\n")
cat("  - property_profile_scored.csv\n")
cat("  - verep_top_opportunities.csv\n")