# ========================================
# GEOCODE MISSING ADDRESSES WITH GEOCODIO
# ========================================

library(tidyverse)
library(geocodio)

Sys.setenv(GEOCODIO_API_KEY = "7199941c634ed174fc7dd93c066c06c0990c466")


# ========================================
# LOAD PROPERTIES NEEDING GEOCODING
# ========================================

properties_to_geocode <- read_csv("data/output/properties_needing_geocoding.csv")

cat(sprintf("Properties to geocode: %d\n", nrow(properties_to_geocode)))

# ========================================
# GEOCODE WITH GEOCODIO
# ========================================

if(nrow(properties_to_geocode) > 0) {
  
  cat("Geocoding with Geocodio...\n")
  
  geocoded <- properties_to_geocode %>%
    rowwise() %>%
    mutate(
      # Build address string
      full_address = case_when(
        # If we have a real address
        !is.na(sadd) & sadd != "" & !grepl("UNASSIGNED", sadd, ignore.case = TRUE) 
        ~ paste(sadd, scity, sstate, szip),
        # Otherwise use city/county
        !is.na(scity) & scity != "" 
        ~ paste(scity, scounty, sstate),
        # Fallback to just county
        TRUE 
        ~ paste(scounty, sstate)
      ),
      
      # Geocode (returns list with lat, lng, accuracy)
      geocode_result = list(tryCatch(
        gio_geocode(full_address),
        error = function(e) list(lat = NA, lng = NA, accuracy = NA, accuracy_type = "failed")
      )),
      
      # Extract results
      geocoded_lat = geocode_result$lat,
      geocoded_lon = geocode_result$lng,
      geocode_accuracy = geocode_result$accuracy,
      geocode_accuracy_type = geocode_result$accuracy_type
    ) %>%
    ungroup()
  
  # ========================================
  # CHECK FOR HISTORIC/CEMETERY INDICATORS
  # ========================================
  
  geocoded <- geocoded %>%
    mutate(
      # Historic indicators
      has_historic_keyword = grepl(
        "historic|old|\\b17\\d{2}\\b|\\b18\\d{2}\\b|parish|colonial", 
        paste(congr_name, sadd, scity), 
        ignore.case = TRUE
      ),
      
      # Cemetery indicators
      is_primarily_cemetery = cemetery == 1 & (church == 0 | is.na(church) | rgisacre > 5),
      is_mixed_cemetery = cemetery == 1 & church == 1 & rgisacre <= 5,
      
      # Constraint scores
      historic_constraint = if_else(has_historic_keyword, 20, 0),
      cemetery_constraint = case_when(
        is_primarily_cemetery ~ 40,
        is_mixed_cemetery ~ 25,
        cemetery == 1 ~ 15,
        TRUE ~ 0
      ),
      combined_constraint = pmin(historic_constraint + cemetery_constraint, 50),
      
      # Needs manual review?
      needs_manual_review = combined_constraint > 0 | 
        geocode_accuracy < 0.8 |
        is.na(geocoded_lat)
    )
  
  # ========================================
  # EXPORT RESULTS
  # ========================================
  
  # All geocoded properties
  write_csv(geocoded, "data/output/geocoded_properties.csv")
  cat("✓ Exported data/output/geocoded_properties.csv\n")
  
  # Properties needing manual review
  manual_review <- geocoded %>%
    filter(needs_manual_review) %>%
    arrange(desc(combined_constraint), desc(lan_val)) %>%
    mutate(
      google_maps_url = paste0("https://www.google.com/maps/search/", 
                               URLencode(full_address)),
      diocese_url = paste0("https://www.thediocese.net/search?q=", 
                           URLencode(paste(congr_name, scity)))
    )
  
  write_csv(manual_review, "data/output/geocoded_needs_manual_review.csv")
  cat(sprintf("✓ Exported %d properties needing manual review\n", nrow(manual_review)))
  
  # Summary
  cat("\n=== GEOCODING SUMMARY ===\n")
  cat(sprintf("Total processed: %d\n", nrow(geocoded)))
  cat(sprintf("Successfully geocoded: %d\n", sum(!is.na(geocoded$geocoded_lat))))
  cat(sprintf("Failed: %d\n", sum(is.na(geocoded$geocoded_lat))))
  cat(sprintf("High accuracy (>0.8): %d\n", sum(geocoded$geocode_accuracy > 0.8, na.rm = TRUE)))
  cat(sprintf("Needs manual review: %d\n", nrow(manual_review)))
  cat(sprintf("Historic indicators: %d\n", sum(geocoded$has_historic_keyword, na.rm = TRUE)))
  cat(sprintf("Primary cemeteries: %d\n", sum(geocoded$is_primarily_cemetery, na.rm = TRUE)))
  
} else {
  cat("No properties need geocoding\n")
}