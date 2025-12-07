# ========================================
# CORRECTED PROPERTY PROFILE GENERATION
# Creates property_profile.rds with 104 unique properties
# ========================================

library(tidyverse)
library(readr)

# ========================================
## STEP 1: LOAD RAW DATA
# ========================================

verep_data <- read_csv("data/verep040725.csv")
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
    
    # Data quality
    data_completeness,
    missing_land_value, missing_acreage, missing_wetland,
    missing_flood, missing_qct, missing_zoning,
    
    # Location
    sadd, scity, scounty, sstate, szip,
    lat, lon,
    
    # Keep clean names for debugging
    clean_name, clean_city
  ) %>%
  arrange(desc(developable), desc(lan_val))

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


# ===========================================
# STEP 10: PULLING OUT PROPERTIES NOT MATCHED TO CONGREGATION DATA
# ============================================

# Properties that didn't match to congregation data
unmatched_properties <- verep_joined %>%
  filter(is.na(attendance_2023))

cat("=== UNMATCHED PROPERTIES SUMMARY ===\n")
cat(sprintf("Total unmatched: %d\n", nrow(unmatched_properties)))

# ----------------------------------------
# Category 1: No congregation assigned (NA congr_name)
# ----------------------------------------
no_congregation <- unmatched_properties %>%
  filter(is.na(congr_name)) %>%
  select(uid, pid, sadd, scity, scounty, lan_val, rgisacre, lat, lon)

cat(sprintf("\nCategory 1 - No congregation assigned: %d\n", nrow(no_congregation)))

# ----------------------------------------
# Category 2: Has congregation name but didn't match
# ----------------------------------------
no_match <- unmatched_properties %>%
  filter(!is.na(congr_name)) %>%
  select(uid, pid, congr_name, clean_name, clean_city, 
         sadd, scity, scounty, lan_val, rgisacre, lat, lon)

cat(sprintf("Category 2 - Name didn't match: %d\n", nrow(no_match)))

# ----------------------------------------
# Diagnose WHY names didn't match
# ----------------------------------------
cat("\n=== DIAGNOSING NON-MATCHES ===\n")

# Get unique unmatched congregation names
unmatched_names <- no_match %>%
  distinct(congr_name, clean_name, clean_city) %>%
  arrange(clean_name)

cat(sprintf("Unique congregation names that didn't match: %d\n", nrow(unmatched_names)))

# Check if these names exist in congregation data (maybe city mismatch?)
congregation_names_available <- congregation_data %>%
  distinct(clean_name, clean_city, name)

# Find near-matches (name exists but city doesn't match)
name_exists_city_different <- unmatched_names %>%
  inner_join(
    congregation_names_available %>% select(clean_name) %>% distinct(),
    by = "clean_name"
  )

cat(sprintf("\nName exists but city didn't match: %d\n", nrow(name_exists_city_different)))

# Names that don't exist at all in congregation data
name_not_found <- unmatched_names %>%
  anti_join(
    congregation_names_available %>% select(clean_name) %>% distinct(),
    by = "clean_name"
  )

cat(sprintf("Name not found in congregation data at all: %d\n", nrow(name_not_found)))

# ----------------------------------------
# Show details
# ----------------------------------------
cat("\n=== NAMES THAT EXIST BUT CITY DIDN'T MATCH ===\n")
if(nrow(name_exists_city_different) > 0) {
  name_exists_city_different %>%
    left_join(
      congregation_names_available %>% 
        group_by(clean_name) %>%
        summarise(available_cities = paste(clean_city, collapse = ", "), .groups = "drop"),
      by = "clean_name"
    ) %>%
    select(congr_name, clean_name, clean_city, available_cities) %>%
    print(n = 30)
}

cat("\n=== NAMES NOT FOUND IN CONGREGATION DATA ===\n")
if(nrow(name_not_found) > 0) {
  print(name_not_found, n = 50)
}

# ----------------------------------------
# Export for review
# ----------------------------------------
dir.create("data/output", recursive = TRUE, showWarnings = FALSE)

write_csv(no_congregation, "data/output/unmatched_no_congregation.csv")
cat("\n✓ Exported unmatched_no_congregation.csv\n")

write_csv(no_match, "data/output/unmatched_name_not_found.csv")
cat("✓ Exported unmatched_name_not_found.csv\n")

if(nrow(name_exists_city_different) > 0) {
  write_csv(name_exists_city_different, "data/output/unmatched_city_mismatch.csv")
  cat("✓ Exported unmatched_city_mismatch.csv\n")
}

if(nrow(name_not_found) > 0) {
  write_csv(name_not_found, "data/output/unmatched_name_missing.csv")
  cat("✓ Exported unmatched_name_missing.csv\n")
}

# ----------------------------------------
# Summary
# ----------------------------------------
cat("\n===========================================\n")
cat("UNMATCHED PROPERTIES BREAKDOWN\n")
cat("===========================================\n")
cat(sprintf("Total VEREP properties: %d\n", nrow(verep_joined)))
cat(sprintf("Matched to congregation: %d\n", sum(!is.na(verep_joined$attendance_2023))))
cat(sprintf("Unmatched: %d\n", nrow(unmatched_properties)))
cat("-------------------------------------------\n")
cat(sprintf("  No congregation assigned (NA): %d\n", nrow(no_congregation)))
cat(sprintf("  Has name but didn't match: %d\n", nrow(no_match)))
cat(sprintf("    - Name exists, city mismatch: %d\n", nrow(name_exists_city_different)))
cat(sprintf("    - Name not in congregation data: %d\n", nrow(name_not_found)))
cat("===========================================\n")