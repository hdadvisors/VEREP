# ========================================
# DATA CREATION - CLEAN DATA ONLY
# NO SCORING - Just prepare clean dataset
# ========================================

library(tidyverse)
library(readr)

# ========================================
# STEP 1-5: Load, Clean, Join, Filter
# ========================================

verep_data <- read_csv("data/congregation_owned_parcels.csv", show_col_types = FALSE)
congregation_data <- read_csv("data/congregation.csv", show_col_types = FALSE)

# Clean names (same as before)
verep_data <- verep_data %>%
  mutate(
    clean_name = str_to_lower(congr_name),
    clean_name = str_replace_all(clean_name, "\\s*\\([^)]+\\)", ""),
    clean_name = str_trim(clean_name),
    clean_city = str_to_lower(str_trim(scity))
  )

congregation_data <- congregation_data %>%
  mutate(
    clean_name = str_to_lower(name),
    clean_name = str_replace_all(clean_name, "\\s*\\([^)]+\\)", ""),
    clean_name = str_trim(clean_name),
    clean_city = str_to_lower(str_trim(city))
  )

# Prepare attendance data
attendance_data <- congregation_data %>%
  select(
    clean_name, clean_city,
    congregation_name = name,
    short_name,
    attendance_2023 = f2023_sunday_attendance,
    attendance_2014 = f2014_sunday_attendance,
    attendance_avg,
    members_2023 = f2023_members_this_year,
    plate_pledge_2023 = f2023_plate_and_pledge,
    plate_pledge_2014 = f2014_plate_and_pledge,
    attendance_pctinc,
    dio_reg
  )

# Join and filter
verep_joined <- verep_data %>%
  left_join(attendance_data, by = c("clean_name", "clean_city"))

analysis_subset <- verep_joined %>%
  filter(!is.na(attendance_2023) & attendance_2023 < 30)

# ========================================
# STEP 6: CREATE PROPERTY PROFILE WITH ALL FIELDS
# ========================================

property_profile <- analysis_subset %>%
  mutate(
    # ========================================
    # ENHANCED ENVIRONMENTAL CONSTRAINTS
    # ========================================
    
    # Individual constraint flags
    has_wetland_constraint = (!is.na(wet_perc) & wet_perc > 10),
    has_flood_constraint = (fema_fz %in% c("A", "AE", "AO", "AH", "V", "VE")),
    has_historic_constraint = (!is.na(hist_dist) & hist_dist == 1),
    has_high_hazard_risk = (!is.na(fema_nri) & fema_nri %in% c("VERY HIGH", "RELATIVELY HIGH")),
    has_moderate_hazard_risk = (!is.na(fema_nri) & fema_nri == "RELATIVELY MODERATE"),
    
    # Calculate constraint penalty (for scoring in common.R)
    constraint_penalty = case_when(
      # Severe - deal breakers
      (!is.na(fema_nri) & fema_nri == "VERY HIGH") ~ -50,
      
      # Multiple major constraints
      (has_wetland_constraint & wet_perc > 25) & has_flood_constraint ~ -75,
      has_flood_constraint & has_high_hazard_risk ~ -70,
      
      # Single major constraints
      has_high_hazard_risk ~ -40,
      has_flood_constraint ~ -40,
      (has_wetland_constraint & wet_perc > 25) ~ -35,
      
      # Moderate constraints
      has_historic_constraint ~ -25,
      (has_wetland_constraint & wet_perc > 10) ~ -20,
      has_moderate_hazard_risk ~ -15,
      
      # No constraints
      TRUE ~ 0
    ),
    
    # Overall environmental constraint flag (for goldilocks classification)
    has_environmental_constraint = (
      has_wetland_constraint | 
        has_flood_constraint | 
        has_high_hazard_risk
    ),
    
    # ========================================
    # DERIVED CALCULATIONS
    # ========================================
    
    # Estimate land value for missing properties
    estimated_land_value = case_when(
      # Use actual land value if available
      !is.na(lan_val) ~ lan_val,
      
      # Estimate from area median home value (100% coverage for missing)
      !is.na(md_h_vl) & !is.na(rgisacre) ~ 
        (md_h_vl * 0.25) * rgisacre,  # Assume land = 25% of home value
      
      # Fallback (shouldn't happen)
      TRUE ~ NA_real_
    ),
    
    land_value_source = if_else(
      !is.na(lan_val), 
      "County Assessor", 
      "Estimated from Area Median"
    ),
    
    # Calculate per-acre value using estimated value
    land_value_per_acre = if_else(
      rgisacre > 0, 
      estimated_land_value / rgisacre, 
      NA_real_
    ),
    
    pct_change_attendance = if_else(
      !is.na(attendance_2014) & attendance_2014 > 0,
      (attendance_2023 - attendance_2014) / attendance_2014 * 100,
      NA_real_
    ),
    
    pct_change_pledge = if_else(
      !is.na(plate_pledge_2014) & plate_pledge_2014 > 0,
      (plate_pledge_2023 - plate_pledge_2014) / plate_pledge_2014 * 100,
      NA_real_
    ),
    
    # ========================================
    # DATA COMPLETENESS
    # ========================================
    
    missing_land_value = is.na(estimated_land_value),
    missing_acreage = is.na(rgisacre),
    missing_wetland = is.na(wet_perc),
    missing_flood = is.na(fema_fz) | fema_fz == "",
    missing_qct = is.na(qct),
    missing_zoning = is.na(zon) | zon == "",
    
    data_completeness = 6 - (
      as.integer(missing_land_value) +
        as.integer(missing_acreage) +
        as.integer(missing_wetland) +
        as.integer(missing_flood) +
        as.integer(missing_qct) +
        as.integer(missing_zoning)
    ),
    
    # ========================================
    # GOLDILOCKS CLASSIFICATION
    # ========================================
    
    development_potential = case_when(
      # HIGH: Goldilocks sweet spot (1.5-5 acres), minimal constraints, documented value
      (rgisacre >= 1.5 & rgisacre <= 5) & !has_environmental_constraint & !is.na(estimated_land_value) ~ "High",  
      
      # MODERATE: Smaller goldilocks range (0.5-1.5 acres), minimal constraints
      (rgisacre >= 0.5 & rgisacre < 1.5) & !has_environmental_constraint & !is.na(estimated_land_value) ~ "Moderate",  
      
      # TOO LARGE: >5 acres (financially unwieldy)
      rgisacre > 5 ~ "Too Large",
      
      # CONSTRAINED: Environmental or hazard issues
      has_environmental_constraint == TRUE ~ "Constrained",
      
      # SMALL PARCEL: <0.5 acres (not economically viable)
      rgisacre < 0.5 ~ "Small Parcel",
      
      # REVIEW NEEDED: Missing data or edge cases
      TRUE ~ "Review Needed"
    )
  ) %>%
  select(
    # Identifiers
    uid, pid, congregation_name, congr_name,
    
    # Attendance & financials
    attendance_2023, members_2023, attendance_avg, attendance_2014,
    pct_change_attendance,
    plate_pledge_2023, plate_pledge_2014, pct_change_pledge,
    
    # Property values
    lan_val, estimated_land_value, land_value_source, land_value_per_acre,  # <-- ADDED estimated fields
    rgisacre, deed_acres,
    
    # Zoning
    zon, zon_desc, zon_type,
    
    # Opportunity zones
    qct, dda, qoz,
    
    # Market demographics 
    medinc, hhi_g_n5, pop_g_n5, h_g_n5, md_h_vl,
    
    # Environmental constraints (detailed)
    wet_perc, fema_fz, fema_nri,
    hist_dist,
    has_wetland_constraint, has_flood_constraint, 
    has_historic_constraint, has_high_hazard_risk, has_moderate_hazard_risk,
    constraint_penalty, has_environmental_constraint,
    
    # Land use flags
    church, cemetery, school, parking, open_space, residence,
    
    # Walkability
    walk_idx,
    
    # Data quality
    data_completeness,
    missing_land_value, missing_acreage, missing_wetland,
    missing_flood, missing_qct, missing_zoning,
    
    # Classification
    development_potential,
    
    # Location
    sadd, scity, scounty, sstate, szip,
    lat, lon,
    
    # Keep clean names
    clean_name, clean_city
  )

# ========================================
# STEP 7B: Identify properties needing geocoding
# ========================================

properties_needing_geocoding <- property_profile %>%
  filter(
    is.na(sadd) | sadd == "" | 
      grepl("UNASSIGNED", sadd, ignore.case = TRUE) |
      is.na(lat) | is.na(lon)
  ) %>%
  select(
    uid, pid, congregation_name, congr_name,
    sadd, scity, scounty, sstate, szip,
    lat, lon,
    attendance_2023, lan_val, rgisacre,
    church, cemetery, school, parking, open_space, residence
  )

write_csv(properties_needing_geocoding, "data/output/properties_needing_geocoding.csv")

# ========================================
# STEP 8: Save clean data
# ========================================

dir.create("data/output", recursive = TRUE, showWarnings = FALSE)

saveRDS(property_profile, "data/output/property_profile.rds")
write_csv(property_profile, "data/output/property_profile.csv")

cat("✓ Saved clean property_profile (104 properties)\n")
cat("✓ No scoring applied - see common.R for development scoring\n")

