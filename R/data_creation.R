# ========================================
# CORRECTED PROPERTY PROFILE GENERATION
# Creates property_profile.rds with 104 unique properties
# ========================================

library(tidyverse)
library(readr)

# ========================================
## STEP 1: LOAD RAW DATA
# ========================================

verep_data <- read_csv("data/congregation_owned_parcels.csv")
congregation_data <- read_csv("data/congregation.csv")

cat("=== RAW DATA LOADED ===\n")
cat(sprintf("VEREP properties: %d\n", nrow(verep_data)))
cat(sprintf("Congregations: %d\n", nrow(congregation_data)))

# ========================================
# STEP 2: CLEAN NAMES AND CITIES (BOTH DATASETS)
# ========================================

# Clean VEREP - remove ALL parenthetical content
verep_data <- verep_data %>%
  mutate(
    clean_name = str_to_lower(congr_name),
    clean_name = str_replace_all(clean_name, "\\s*\\([^)]+\\)", ""),
    clean_name = str_trim(clean_name),
    clean_city = str_to_lower(str_trim(scity))
  )

# Clean congregation - ALSO remove parentheses (this was the bug)
congregation_data <- congregation_data %>%
  mutate(
    clean_name = str_to_lower(name),
    clean_name = str_replace_all(clean_name, "\\s*\\([^)]+\\)", ""),
    clean_name = str_trim(clean_name),
    clean_city = str_to_lower(str_trim(city))
  )

# ========================================
# STEP 3: PREPARE ATTENDANCE DATA
# ========================================

attendance_data <- congregation_data %>%
  select(
    clean_name,
    clean_city,
    congregation_name = name,  # Keep original name for display
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

# ========================================
# STEP 4: JOIN ON CLEAN_NAME + CLEAN_CITY
# ========================================

verep_joined <- verep_data %>%
  left_join(attendance_data, by = c("clean_name", "clean_city"))

# Verify no many-to-many issues
cat("\n=== JOIN RESULTS ===\n")
cat(sprintf("Rows after join: %d (should equal VEREP rows: %d)\n", 
            nrow(verep_joined), nrow(verep_data)))
cat(sprintf("Properties with attendance match: %d\n", 
            sum(!is.na(verep_joined$attendance_2023))))

# ========================================
# STEP 5: FILTER FOR <30 ATTENDANCE
# ========================================

analysis_subset <- verep_joined %>%
  filter(
    !is.na(attendance_2023) &
      attendance_2023 < 30
  )

cat(sprintf("\nProperties with <30 attendance: %d\n", nrow(analysis_subset)))
cat(sprintf("Unique UIDs: %d\n", n_distinct(analysis_subset$uid)))

# Verify no duplicates
if(nrow(analysis_subset) != n_distinct(analysis_subset$uid)) {
  warning("DUPLICATE UIDs DETECTED - check join logic!")
} else {
  cat("✓ No duplicate UIDs - data is clean\n")
}

# ========================================
# STEP 6: CREATE PROPERTY PROFILE WITH ALL FIELDS
# ========================================

property_profile <- analysis_subset %>%
  mutate(
    # Environmental constraints
    has_environmental_constraint = (wet_perc > 10) | 
      (fema_fz %in% c("A", "AE", "AO", "AH", "V", "VE")),
    
    # Developability flag
    developable = (qct == 1) & (rgisacre >= 1) & 
      (!has_environmental_constraint | is.na(has_environmental_constraint)),
    
    # Land value per acre
    land_value_per_acre = if_else(rgisacre > 0, lan_val / rgisacre, NA_real_),
    
    # Percent change calculations
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
    
    # Data completeness flags
    missing_land_value = is.na(lan_val),
    missing_acreage = is.na(rgisacre),
    missing_wetland = is.na(wet_perc),
    missing_flood = is.na(fema_fz) | fema_fz == "",
    missing_qct = is.na(qct),
    missing_zoning = is.na(zon) | zon == "",
    
    # Data completeness score (0-6)
    data_completeness = 6 - (
      as.integer(missing_land_value) +
        as.integer(missing_acreage) +
        as.integer(missing_wetland) +
        as.integer(missing_flood) +
        as.integer(missing_qct) +
        as.integer(missing_zoning)
    ),
    
    # Development potential category
    development_potential = case_when(
      developable == TRUE ~ "High",
      (qct == 1 | dda == 1) & rgisacre >= 0.5 & rgisacre < 1 ~ "Medium",
      has_environmental_constraint == TRUE ~ "Constrained",
      rgisacre < 0.5 ~ "Small Parcel",
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
    lan_val, land_value_per_acre, rgisacre, deed_acres,
    
    # Zoning
    zon, zon_desc, zon_type,
    
    # Opportunity zones
    qct, dda,
    
    # Environmental
    wet_perc, fema_fz, fema_nri,
    has_environmental_constraint, developable, development_potential,
    
    # Land use flags  <-- ADD THIS SECTION
    church, cemetery, school, parking, open_space, residence,
    
    # Walkability  <-- ADD THIS TOO if you need it
    walk_idx,
    
    # Data quality
    data_completeness,
    missing_land_value, missing_acreage, missing_wetland,
    missing_flood, missing_qct, missing_zoning,
    
    # Location
    sadd, scity, scounty, sstate, szip,
    lat, lon,
    
    # Keep clean names for debugging
    clean_name, clean_city
  )

# ========================================
# STEP 7: FINAL VALIDATION
# ========================================

cat("\n=== FINAL PROPERTY PROFILE ===\n")
cat(sprintf("Total rows: %d\n", nrow(property_profile)))
cat(sprintf("Unique UIDs: %d\n", n_distinct(property_profile$uid)))
cat(sprintf("Unique PIDs: %d\n", n_distinct(property_profile$pid)))
cat(sprintf("Properties with land value: %d\n", sum(!is.na(property_profile$lan_val))))
cat(sprintf("Properties missing land value: %d\n", sum(is.na(property_profile$lan_val))))
cat(sprintf("Data completeness range: %d - %d\n", 
            min(property_profile$data_completeness, na.rm = TRUE),
            max(property_profile$data_completeness, na.rm = TRUE)))

# ========================================
# STEP 7B: GEOCODING & CONSTRAINT IDENTIFICATION
# ========================================
cat("\n=== IDENTIFYING PROPERTIES NEEDING GEOCODING ===\n")

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
    church, cemetery, school, parking, open_space, residence,
    development_potential
  )

cat(sprintf("Properties needing geocoding: %d\n", nrow(properties_needing_geocoding)))

# Export for geocoding
write_csv(properties_needing_geocoding, 
          "data/output/properties_needing_geocoding.csv")
cat("✓ Exported data/output/properties_needing_geocoding.csv\n")
  
# ========================================
# MERGE GEOCODED DATA BACK
# ========================================

library(tidyverse)

# Load property profile
property_profile <- readRDS("data/output/property_profile.rds")

# Load geocoded results
geocoded <- read_csv("data/output/geocoded_properties.csv")

# Merge
property_profile_updated <- property_profile %>%
  left_join(
    geocoded %>% 
      select(uid, geocoded_lat, geocoded_lon, geocode_accuracy, 
             geocode_accuracy_type, has_historic_keyword, 
             is_primarily_cemetery, combined_constraint),
    by = "uid"
  ) %>%
  mutate(
    # Use geocoded coordinates if original missing
    lat = coalesce(lat, geocoded_lat),
    lon = coalesce(lon, geocoded_lon)
  )

# Save updated profile
saveRDS(property_profile_updated, "data/output/property_profile.rds")
write_csv(property_profile_updated, "data/output/property_profile.csv")

cat("✓ Merged geocoding results back into property_profile\n")

# ========================================
# STEP 8: SAVE OUTPUT
# ========================================

dir.create("data/output", recursive = TRUE, showWarnings = FALSE)

# Save as RDS (preserves data types)
saveRDS(property_profile, "data/output/property_profile.rds")
cat("\n✓ Saved property_profile.rds\n")

# Also save as CSV for inspection
write_csv(property_profile, "data/output/property_profile.csv")
cat("✓ Saved property_profile.csv\n")

# ========================================
# STEP 9: SUMMARY STATISTICS
# ========================================

cat("\n===========================================\n")
cat("PROPERTY PROFILE GENERATION COMPLETE\n")
cat("===========================================\n")
cat(sprintf("Total properties: %d\n", nrow(property_profile)))
cat(sprintf("Unique congregations: %d\n", n_distinct(property_profile$congregation_name)))
cat(sprintf("Average attendance: %.1f\n", mean(property_profile$attendance_2023, na.rm = TRUE)))
cat(sprintf("Properties in QCT: %d (%.1f%%)\n", 
            sum(property_profile$qct == 1, na.rm = TRUE),
            sum(property_profile$qct == 1, na.rm = TRUE) / nrow(property_profile) * 100))
cat(sprintf("High development potential: %d\n", 
            sum(property_profile$development_potential == "High", na.rm = TRUE)))
cat(sprintf("Total land value: %s\n", 
            scales::dollar(sum(property_profile$lan_val, na.rm = TRUE))))
cat(sprintf("Total acreage: %.2f\n", 
            sum(property_profile$rgisacre, na.rm = TRUE)))
cat("===========================================\n")


