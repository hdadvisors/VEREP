
# ========================================
# GEOCODE WITH GEOCODIO REST API
# Batch geocodes properties missing addresses/coordinates
# ========================================

library(tidyverse)
library(httr)
library(jsonlite)

# ========================================
# CONFIGURATION
# ========================================

# Get your free API key at: https://www.geocod.io/
GEOCODIO_API_KEY <- "7199941c634ed174fc7dd93c066c06c0990c466"

# ========================================
# BATCH GEOCODING FUNCTION
# ========================================

batch_geocode_geocodio <- function(addresses, api_key = GEOCODIO_API_KEY) {
  
  cat(sprintf("Sending %d addresses to Geocodio...\n", length(addresses)))
  
  url <- "https://api.geocod.io/v1.9/geocode"
  
  # Make batch request
  response <- POST(
    url,
    query = list(api_key = api_key),
    body = toJSON(addresses, auto_unbox = TRUE),
    encode = "json",
    content_type_json()
  )
  
  # Check for errors
  if (status_code(response) != 200) {
    error_msg <- content(response, as = "text")
    stop(sprintf("Geocodio API error: %s", error_msg))
  }
  
  # Parse results
  result <- content(response, as = "parsed")
  
  # Extract coordinates from each result
  results_df <- map_df(seq_along(result$results), function(i) {
    r <- result$results[[i]]
    
    if (length(r$response$results) > 0) {
      best <- r$response$results[[1]]
      tibble(
        address_index = i,
        query = r$query,
        geocoded_lat = best$location$lat,
        geocoded_lon = best$location$lng,
        geocode_accuracy = best$accuracy,
        geocode_accuracy_type = best$accuracy_type,
        geocode_source = "Geocodio"
      )
    } else {
      tibble(
        address_index = i,
        query = r$query,
        geocoded_lat = NA_real_,
        geocoded_lon = NA_real_,
        geocode_accuracy = NA_real_,
        geocode_accuracy_type = "no_results",
        geocode_source = "Geocodio"
      )
    }
  })
  
  return(results_df)
}

# ========================================
# LOAD PROPERTIES NEEDING GEOCODING
# ========================================

cat("\n=== LOADING PROPERTIES ===\n")

if (!file.exists("data/output/properties_needing_geocoding.csv")) {
  stop("File not found: data/output/properties_needing_geocoding.csv\n",
       "Run data_creation.R first to generate this file.")
}

properties_to_geocode <- read_csv("data/output/properties_needing_geocoding.csv",
                                  show_col_types = FALSE)

cat(sprintf("Properties needing geocoding: %d\n", nrow(properties_to_geocode)))

# ========================================
# GEOCODE PROPERTIES
# ========================================

if(nrow(properties_to_geocode) > 0) {
  
  cat("\n=== GEOCODING WITH GEOCODIO ===\n")
  
  # Build address strings for geocoding
  properties_with_addresses <- properties_to_geocode %>%
    mutate(
      address_index = row_number(),
      full_address = case_when(
        # Strategy 1: Full street address
        !is.na(sadd) & sadd != "" & !grepl("UNASSIGNED", sadd, ignore.case = TRUE) 
        ~ paste(sadd, scity, sstate, szip),
        
        # Strategy 2: City, County, State
        !is.na(scity) & scity != "" 
        ~ paste(scity, scounty, sstate),
        
        # Strategy 3: County, State only
        TRUE 
        ~ paste(scounty, sstate)
      )
    )
  
  # Batch geocode all addresses
  geocode_results <- batch_geocode_geocodio(properties_with_addresses$full_address)
  
  cat(sprintf("✓ Geocoded %d addresses\n", nrow(geocode_results)))
  
  # Merge results back
  geocoded <- properties_with_addresses %>%
    left_join(geocode_results, by = "address_index") %>%
    select(-address_index)
  
  # ========================================
  # CHECK FOR CONSTRAINTS
  # ========================================
  
  cat("\n=== CHECKING FOR CONSTRAINTS ===\n")
  
  geocoded <- geocoded %>%
    mutate(
      # Historic indicators (keywords in name/address)
      has_historic_keyword = grepl(
        "historic|old|\\b17\\d{2}\\b|\\b18\\d{2}\\b|parish|colonial|ancient|original", 
        paste(congr_name, sadd, scity), 
        ignore.case = TRUE
      ),
      
      # Cemetery type identification
      is_primarily_cemetery = cemetery == 1 & (church == 0 | is.na(church) | rgisacre > 5),
      is_mixed_cemetery = cemetery == 1 & church == 1 & rgisacre <= 5,
      has_cemetery_component = cemetery == 1,
      
      # Constraint scores
      historic_constraint = if_else(has_historic_keyword, 20, 0),
      cemetery_constraint = case_when(
        is_primarily_cemetery ~ 40,
        is_mixed_cemetery ~ 25,
        has_cemetery_component ~ 15,
        TRUE ~ 0
      ),
      combined_constraint = pmin(historic_constraint + cemetery_constraint, 50),
      
      # Development potential adjustment
      development_potential_adjusted = case_when(
        combined_constraint >= 40 ~ "Severely Constrained",
        combined_constraint >= 25 ~ "Highly Constrained",
        combined_constraint >= 10 ~ "Moderately Constrained",
        TRUE ~ development_potential
      ),
      
      # Flag for manual review
      needs_manual_review = combined_constraint > 0 | 
        geocode_accuracy < 0.8 |
        is.na(geocoded_lat)
    )
  
  # Summary counts
  cat(sprintf("Historic indicators found: %d\n", sum(geocoded$has_historic_keyword, na.rm = TRUE)))
  cat(sprintf("Primary cemeteries: %d\n", sum(geocoded$is_primarily_cemetery, na.rm = TRUE)))
  cat(sprintf("Mixed cemeteries: %d\n", sum(geocoded$is_mixed_cemetery, na.rm = TRUE)))
  
  # ========================================
  # GENERATE SEARCH URLS
  # ========================================
  
  geocoded <- geocoded %>%
    mutate(
      google_maps_url = paste0("https://www.google.com/maps/search/", 
                               URLencode(full_address)),
      diocese_url = paste0("https://www.thediocese.net/search?q=", 
                           URLencode(paste(congr_name, scity))),
      findagrave_url = paste0("https://www.findagrave.com/cemetery-browse?cemetery=", 
                              URLencode(congr_name)),
      hmdb_url = paste0("https://www.hmdb.org/results.asp?search=", 
                        URLencode(paste(congr_name, scity)))
    )
  
  # ========================================
  # EXPORT RESULTS
  # ========================================
  
  cat("\n=== EXPORTING RESULTS ===\n")
  
  # 1. All geocoded properties (for merging back)
  geocoded_for_merge <- geocoded %>%
    select(
      uid, 
      geocoded_lat, geocoded_lon, geocode_accuracy, geocode_accuracy_type, geocode_source,
      has_historic_keyword, historic_constraint,
      is_primarily_cemetery, is_mixed_cemetery, has_cemetery_component, cemetery_constraint,
      combined_constraint, development_potential_adjusted
    )
  
  write_csv(geocoded_for_merge, "data/output/geocoded_properties.csv")
  cat("✓ Exported data/output/geocoded_properties.csv\n")
  
  # 2. Properties needing manual review
  manual_review <- geocoded %>%
    filter(needs_manual_review) %>%
    arrange(desc(combined_constraint), desc(lan_val)) %>%
    select(
      uid, congregation_name, congr_name,
      sadd, scity, scounty, full_address,
      attendance_2023, lan_val, rgisacre,
      geocoded_lat, geocoded_lon, geocode_accuracy, geocode_accuracy_type,
      combined_constraint, historic_constraint, cemetery_constraint,
      has_historic_keyword, is_primarily_cemetery, is_mixed_cemetery,
      development_potential, development_potential_adjusted,
      google_maps_url, diocese_url, findagrave_url, hmdb_url
    )
  
  write_csv(manual_review, "data/output/geocoded_needs_manual_review.csv")
  cat(sprintf("✓ Exported %d properties needing manual review\n", nrow(manual_review)))
  
  # 3. Summary statistics
  geocoding_summary <- tibble(
    total_properties = nrow(geocoded),
    geocode_success = sum(!is.na(geocoded$geocoded_lat)),
    geocode_failed = sum(is.na(geocoded$geocoded_lat)),
    high_accuracy_count = sum(geocoded$geocode_accuracy > 0.8, na.rm = TRUE),
    medium_accuracy_count = sum(geocoded$geocode_accuracy >= 0.5 & geocoded$geocode_accuracy <= 0.8, na.rm = TRUE),
    low_accuracy_count = sum(geocoded$geocode_accuracy < 0.5, na.rm = TRUE),
    historic_indicators = sum(geocoded$has_historic_keyword, na.rm = TRUE),
    primary_cemeteries = sum(geocoded$is_primarily_cemetery, na.rm = TRUE),
    mixed_cemeteries = sum(geocoded$is_mixed_cemetery, na.rm = TRUE),
    needs_manual_review = nrow(manual_review),
    severely_constrained = sum(geocoded$combined_constraint >= 40, na.rm = TRUE),
    highly_constrained = sum(geocoded$combined_constraint >= 25 & geocoded$combined_constraint < 40, na.rm = TRUE),
    moderately_constrained = sum(geocoded$combined_constraint >= 10 & geocoded$combined_constraint < 25, na.rm = TRUE)
  )
  
  write_csv(geocoding_summary, "data/output/geocoding_summary.csv")
  cat("✓ Exported data/output/geocoding_summary.csv\n")
  
  # ========================================
  # SUMMARY REPORT
  # ========================================
  
  cat("\n===========================================\n")
  cat("GEOCODING COMPLETE\n")
  cat("===========================================\n")
  cat(sprintf("Total properties: %d\n", geocoding_summary$total_properties))
  cat(sprintf("Successfully geocoded: %d (%.1f%%)\n", 
              geocoding_summary$geocode_success,
              geocoding_summary$geocode_success / geocoding_summary$total_properties * 100))
  cat(sprintf("Failed: %d\n", geocoding_summary$geocode_failed))
  cat("-------------------------------------------\n")
  cat("Accuracy:\n")
  cat(sprintf("  High (>0.8): %d\n", geocoding_summary$high_accuracy_count))
  cat(sprintf("  Medium (0.5-0.8): %d\n", geocoding_summary$medium_accuracy_count))
  cat(sprintf("  Low (<0.5): %d\n", geocoding_summary$low_accuracy_count))
  cat("-------------------------------------------\n")
  cat("Constraints:\n")
  cat(sprintf("  Historic indicators: %d\n", geocoding_summary$historic_indicators))
  cat(sprintf("  Primary cemeteries: %d\n", geocoding_summary$primary_cemeteries))
  cat(sprintf("  Mixed cemeteries: %d\n", geocoding_summary$mixed_cemeteries))
  cat(sprintf("  Severely constrained: %d\n", geocoding_summary$severely_constrained))
  cat(sprintf("  Highly constrained: %d\n", geocoding_summary$highly_constrained))
  cat(sprintf("  Moderately constrained: %d\n", geocoding_summary$moderately_constrained))
  cat("-------------------------------------------\n")
  cat(sprintf("Properties needing manual review: %d\n", geocoding_summary$needs_manual_review))
  cat("===========================================\n")
  cat("\nNext steps:\n")
  cat("1. Review: data/output/geocoded_needs_manual_review.csv\n")
  cat("2. Merge results with: source('merge_geocoded_data.R')\n")
  cat("===========================================\n")
  
} else {
  cat("\nNo properties need geocoding\n")
}