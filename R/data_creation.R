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

library(tidygeocoder)

cat("\n=== GEOCODING & CONSTRAINT IDENTIFICATION ===\n")

# ----------------------------------------
# Identify properties needing geocoding
# ----------------------------------------
properties_to_geocode <- property_profile %>%
  filter(
    is.na(sadd) | sadd == "" | 
      grepl("UNASSIGNED", sadd, ignore.case = TRUE) |  # <-- FIX: ADD THIS LINE
      is.na(lat) | is.na(lon)
  )

cat(sprintf("Properties needing geocoding: %d\n", nrow(properties_to_geocode)))

# ----------------------------------------
# Geocoding function with fallback
# ----------------------------------------
geocode_property <- function(address, city, county, state = "VA") {
  
  # Try multiple search strategies
  searches <- list(
    list(query = paste(address, city, state), 
         confidence = "high"),
    list(query = paste(address, county, state), 
         confidence = "medium"),
    list(query = paste(city, county, state), 
         confidence = "low"),
    list(query = paste(county, state), 
         confidence = "very_low")
  )
  
  for(search in searches) {
    tryCatch({
      result <- tidygeocoder::geo(
        search$query,
        method = "osm",
        limit = 1,
        timeout = 10
      )
      
      if(!is.na(result$lat) && !is.na(result$long)) {
        return(list(
          lat = result$lat,
          lon = result$long,
          source = "OSM",
          confidence = search$confidence,
          query_used = search$query
        ))
      }
    }, error = function(e) {
      # Continue to next search strategy
    })
  }
  
  # If all fail, try Census API
  tryCatch({
    result <- tidygeocoder::geo(
      paste(city, county, state),
      method = "census",
      limit = 1
    )
    
    if(!is.na(result$lat) && !is.na(result$long)) {
      return(list(
        lat = result$lat,
        lon = result$long,
        source = "Census",
        confidence = "low",
        query_used = paste(city, county, state)
      ))
    }
  }, error = function(e) {
    # Geocoding completely failed
  })
  
  # Return NAs if all geocoding attempts failed
  return(list(
    lat = NA,
    lon = NA,
    source = NA,
    confidence = "failed",
    query_used = NA
  ))
}

# ----------------------------------------
# Check for historic indicators
# ----------------------------------------
check_historic_status <- function(congr_name, address, city) {
  historic_keywords <- c(
    "historic", "old", "\\b17\\d{2}\\b", "\\b18\\d{2}\\b", 
    "parish", "colonial", "ancient", "original"
  )
  
  text_to_check <- paste(
    tolower(congr_name), 
    tolower(address), 
    tolower(city)
  )
  
  matches <- sum(sapply(historic_keywords, function(kw) {
    grepl(kw, text_to_check, ignore.case = TRUE)
  }))
  
  return(list(
    likely_historic = matches >= 2,
    historic_confidence = case_when(
      matches >= 3 ~ "high",
      matches == 2 ~ "medium",
      matches == 1 ~ "low",
      TRUE ~ "none"
    ),
    historic_check_needed = matches >= 1
  ))
}

# ----------------------------------------
# Check cemetery indicators
# ----------------------------------------
check_cemetery_indicators <- function(cemetery_flag, church_flag, 
                                      open_space_flag, rgisacre) {
  
  # Primary cemetery if:
  # - Cemetery flag AND (not church OR large parcel)
  is_primary_cemetery <- cemetery_flag == 1 & 
    (church_flag == 0 | is.na(church_flag) | rgisacre > 5)
  
  # Mixed use if cemetery + church on smaller parcel
  is_mixed_cemetery <- cemetery_flag == 1 & 
    church_flag == 1 & 
    rgisacre <= 5
  
  # Has cemetery component
  has_cemetery_component <- cemetery_flag == 1
  
  return(list(
    is_primary_cemetery = is_primary_cemetery,
    is_mixed_cemetery = is_mixed_cemetery,
    has_cemetery_component = has_cemetery_component,
    cemetery_confidence = case_when(
      is_primary_cemetery ~ "high",
      is_mixed_cemetery ~ "medium",
      has_cemetery_component ~ "low",
      TRUE ~ "none"
    )
  ))
}

# ----------------------------------------
# Generate search URLs
# ----------------------------------------
generate_search_urls <- function(congr_name, address, city, county, state = "VA") {
  
  # Clean name for URL
  name_clean <- URLencode(paste(congr_name, city, state))
  address_clean <- URLencode(paste(address, city, state))
  
  return(list(
    google_maps = paste0("https://www.google.com/maps/search/", address_clean),
    diocese_search = paste0("https://www.thediocese.net/search?q=", name_clean),
    findagrave = paste0("https://www.findagrave.com/cemetery-browse?cemetery=", 
                        URLencode(congr_name)),
    hmdb = paste0("https://www.hmdb.org/results.asp?search=", name_clean)
  ))
}

# ----------------------------------------
# Process geocoding (in batches to avoid rate limits)
# ----------------------------------------
if(nrow(properties_to_geocode) > 0) {
  
  cat("Processing geocoding...\n")
  
  geocoded_results <- properties_to_geocode %>%
    rowwise() %>%
    mutate(
      # Geocode
      geocode_result = list(geocode_property(sadd, scity, scounty, sstate)),
      geocoded_lat = geocode_result$lat,
      geocoded_lon = geocode_result$lon,
      geocode_source = geocode_result$source,
      geocode_confidence = geocode_result$confidence,
      geocode_query = geocode_result$query_used,
      
      # Historic check
      historic_result = list(check_historic_status(congr_name, sadd, scity)),
      likely_historic = historic_result$likely_historic,
      historic_confidence = historic_result$historic_confidence,
      historic_check_needed = historic_result$historic_check_needed,
      
      # Cemetery check
      cemetery_result = list(check_cemetery_indicators(
        cemetery, church, open_space, rgisacre
      )),
      is_primary_cemetery = cemetery_result$is_primary_cemetery,
      is_mixed_cemetery = cemetery_result$is_mixed_cemetery,
      has_cemetery_component = cemetery_result$has_cemetery_component,
      cemetery_confidence = cemetery_result$cemetery_confidence,
      
      # Search URLs
      search_urls = list(generate_search_urls(congr_name, sadd, scity, scounty, sstate)),
      google_maps_url = search_urls$google_maps,
      diocese_url = search_urls$diocese_search,
      findagrave_url = search_urls$findagrave,
      hmdb_url = search_urls$hmdb
    ) %>%
    ungroup()
  
  # ----------------------------------------
  # Calculate constraint scores
  # ----------------------------------------
  geocoded_results <- geocoded_results %>%
    mutate(
      # Historic constraint score (0-30 points)
      historic_constraint_score = case_when(
        likely_historic & historic_confidence == "high" ~ 30,
        likely_historic & historic_confidence == "medium" ~ 20,
        historic_check_needed ~ 10,
        TRUE ~ 0
      ),
      
      # Cemetery constraint score (0-40 points)
      cemetery_constraint_score = case_when(
        is_primary_cemetery ~ 40,
        is_mixed_cemetery ~ 25,
        has_cemetery_component ~ 15,
        TRUE ~ 0
      ),
      
      # Combined constraint score (capped at 50)
      combined_constraint_score = pmin(
        historic_constraint_score + cemetery_constraint_score, 
        50
      ),
      
      # Adjust development potential
      development_potential_adjusted = case_when(
        combined_constraint_score >= 40 ~ "Severely Constrained",
        combined_constraint_score >= 25 ~ "Highly Constrained",
        combined_constraint_score >= 10 ~ "Moderately Constrained",
        TRUE ~ development_potential
      )
    )
  
  # ----------------------------------------
  # Merge back into property_profile
  # ----------------------------------------
  property_profile <- property_profile %>%
    left_join(
      geocoded_results %>%
        select(uid, geocoded_lat, geocoded_lon, geocode_source, 
               geocode_confidence, likely_historic, historic_confidence,
               historic_check_needed, is_primary_cemetery, is_mixed_cemetery,
               has_cemetery_component, cemetery_confidence,
               historic_constraint_score, cemetery_constraint_score,
               combined_constraint_score, development_potential_adjusted,
               google_maps_url, diocese_url, findagrave_url, hmdb_url),
      by = "uid"
    ) %>%
    mutate(
      # Use geocoded coordinates if original are missing
      lat = coalesce(lat, geocoded_lat),
      lon = coalesce(lon, geocoded_lon)
    )
  
  # ----------------------------------------
  # Export for manual review
  # ----------------------------------------
  properties_needing_review <- property_profile %>%
    filter(
      combined_constraint_score > 0 |
        geocode_confidence %in% c("low", "very_low", "failed")
    ) %>%
    arrange(desc(combined_constraint_score), desc(lan_val)) %>%
    select(
      uid, congregation_name, sadd, scity, scounty,
      attendance_2023, lan_val, rgisacre,
      combined_constraint_score, historic_constraint_score, cemetery_constraint_score,
      likely_historic, historic_confidence,
      is_primary_cemetery, is_mixed_cemetery,
      geocode_confidence,
      development_potential, development_potential_adjusted,
      google_maps_url, diocese_url, findagrave_url, hmdb_url
    )
  
  write_csv(properties_needing_review, 
            "data/output/properties_needing_manual_review.csv")
  cat(sprintf("✓ Exported %d properties needing manual review\n", 
              nrow(properties_needing_review)))
  
  # Summary
  geocoding_summary <- tibble(
    total_geocoded = nrow(geocoded_results),
    geocode_success = sum(!is.na(geocoded_results$geocoded_lat)),
    geocode_failed = sum(is.na(geocoded_results$geocoded_lat)),
    likely_historic = sum(geocoded_results$likely_historic, na.rm = TRUE),
    primary_cemetery = sum(geocoded_results$is_primary_cemetery, na.rm = TRUE),
    needs_manual_review = nrow(properties_needing_review)
  )
  
  write_csv(geocoding_summary, "data/output/geocoding_summary.csv")
  cat("✓ Exported geocoding_summary.csv\n")
  
} else {
  cat("No properties need geocoding\n")
}

cat("\n=== GEOCODING COMPLETE ===\n")

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


