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
# STEP 6: CREATE CLEAN PROPERTY PROFILE
# No scoring - just factual data
# ========================================

property_profile <- analysis_subset %>%
  mutate(
    # Environmental constraints (factual, not scored)
    has_wetland_constraint = (!is.na(wet_perc) & wet_perc > 10),
    has_flood_constraint = (fema_fz %in% c("A", "AE", "AO", "AH", "V", "VE")),
    has_environmental_constraint = has_wetland_constraint | has_flood_constraint,
    
    # Derived calculations
    land_value_per_acre = if_else(rgisacre > 0, lan_val / rgisacre, NA_real_),
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
    
    # Data completeness
    missing_land_value = is.na(lan_val),
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
    lan_val, land_value_per_acre, rgisacre, deed_acres,
    
    # Zoning
    zon, zon_desc, zon_type,
    
    # Opportunity zones
    qct, dda,
    
    # Environmental (factual flags only)
    wet_perc, fema_fz, fema_nri,
    has_wetland_constraint, has_flood_constraint, has_environmental_constraint,
    
    # Land use flags
    church, cemetery, school, parking, open_space, residence,
    
    # Walkability
    walk_idx,
    
    # Data quality
    data_completeness,
    missing_land_value, missing_acreage, missing_wetland,
    missing_flood, missing_qct, missing_zoning,
    
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