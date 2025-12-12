
# PROBLEM PROPERTIES ANALYSIS 
# Analysis of data quality issues for properties with <30 attendees


# SECTION 1: SETUP ----

library(tidyverse)
library(readr)
library(scales)

property_profile <- readRDS("data/output/property_profile.rds")

# Validate row count
cat(sprintf("Loaded %d rows\n", nrow(property_profile)))
cat(sprintf("Unique UIDs: %d\n", n_distinct(property_profile$uid)))

# If duplicates exist, deduplicate or reload from correct source
if(nrow(property_profile) != n_distinct(property_profile$uid, na.rm = TRUE)) {
  
  warning("Duplicate UIDs detected! Consider reloading from analysis_subset.")
}

cat("=== DATA LOADED ===\n")
cat(sprintf("Total properties: %d\n", nrow(property_profile)))
cat(sprintf("Properties with incomplete data (score <6): %d\n\n", 
            sum(property_profile$data_completeness < 6)))


property_profile_improved <- property_profile %>%
  mutate(
    # Safe defaults for environmental data
    wet_perc = replace_na(wet_perc, 0),
    fema_fz = if_else(is.na(fema_fz) | fema_fz == "", "X", fema_fz),
    
    # Recalculate flags
    missing_wetland = FALSE,
    missing_flood = FALSE,
    
    # Recalculate completeness score
    data_completeness = 6 - (
      as.integer(missing_land_value) +
        as.integer(missing_acreage) +
        as.integer(missing_wetland) +
        as.integer(missing_flood) +
        as.integer(missing_qct) +
        as.integer(missing_zoning)
    ),
    
    # Recalculate environmental constraint (now with real values)
    has_environmental_constraint = (wet_perc > 10) | 
      (fema_fz %in% c("A", "AE", "AO", "AH", "V", "VE"))
  )

# Check improvement
cat("Before:", sum(property_profile$data_completeness < 6), "incomplete\n")
cat("After: ", sum(property_profile_improved$data_completeness < 6), "incomplete\n")

write_rds(property_profile_improved, "data/property_profile_improved.rds")

needs_manual_lookup <- property_profile_improved %>%
  filter(missing_land_value | missing_zoning) %>%
  select(congregation_name, sadd, scity, scounty, pid, lat, lon,
         lan_val, zon, missing_land_value, missing_zoning)

write_csv(needs_manual_lookup, "data/output/needs_manual_lookup.csv")


# SECTION 2: DATA COMPLETENESS ANALYSIS ----


cat("\n=== SECTION 2: DATA COMPLETENESS ===\n")

## Identify properties with missing data ----
incomplete_properties <- property_profile_improved %>%
  filter(data_completeness < 6) %>%
  arrange(data_completeness) %>%
  select(congregation_name, data_completeness, sadd, scity, sstate, szip,
         missing_land_value, missing_acreage, missing_wetland, 
         missing_flood, missing_qct, missing_zoning)

## Print summary of worst cases ----
cat(sprintf("\nProperties with incomplete data: %d\n", nrow(incomplete_properties)))
cat("\nTop 10 worst cases:\n")
top_problems <- head(incomplete_properties, 10)
for(i in 1:nrow(top_problems)) {
  missing_fields <- c()
  if(top_problems$missing_land_value[i]) missing_fields <- c(missing_fields, "land value")
  if(top_problems$missing_acreage[i]) missing_fields <- c(missing_fields, "acreage")
  if(top_problems$missing_wetland[i]) missing_fields <- c(missing_fields, "wetlands")
  if(top_problems$missing_flood[i]) missing_fields <- c(missing_fields, "flood zone")
  if(top_problems$missing_qct[i]) missing_fields <- c(missing_fields, "QCT")
  if(top_problems$missing_zoning[i]) missing_fields <- c(missing_fields, "zoning")
  
  cat(sprintf("  • %s (score: %d/6) - Missing: %s\n", 
              top_problems$congregation_name[i],
              top_problems$data_completeness[i],
              paste(missing_fields, collapse = ", ")))
}


# SECTION 3: ADDRESS QUALITY ANALYSIS ----


cat("\n=== SECTION 3: ADDRESS QUALITY ===\n")

## 3A: Unassigned/Missing Addresses ----
unassigned_addresses <- property_profile %>%
  filter(is.na(sadd) | sadd == "" | grepl("UNASSIGNED", sadd, ignore.case = TRUE)) %>%
  group_by(congregation_name) %>%
  summarise(
    num_unassigned = n(),
    unassigned_addresses = paste(unique(sadd[!is.na(sadd) & sadd != ""]), collapse = " | "),
    cities = paste(unique(scity[!is.na(scity)]), collapse = ", "),
    has_coords = sum(!is.na(lat) & !is.na(lon)),
    sample_lat = first(lat[!is.na(lat)]),
    sample_lon = first(lon[!is.na(lon)]),
    .groups = 'drop'
  ) %>%
  arrange(desc(num_unassigned))


### Geocoding unassigned with coordinates ----

unassigned_addresses |>
  select(congregation_name, num_unassigned, cities, has_coords) |>
  mutate(has_coords = if_else(has_coords > 0, "Yes", "No")) |>
  kable(
    col.names = c("Congregation", "# Unassigned Properties", "Cities", "Has Coordinates"),
    digits = 0
  ) |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) |>
  row_spec(which(unassigned_addresses$has_coords == 0), background = "#f8d7da")

library(tidygeocoder)

needs_geocoding <- unassigned_addresses |>
  filter(!is.na(sample_lat) & !is.na(sample_lon))

geocoded_results <- needs_geocoding |>
  reverse_geocode(
    lat = sample_lat,
    long = sample_lon,
    method = "arcgis",
    full_results = FALSE
  )

# Save the geocoded addresses
write_csv(geocoded_results, "data/output/geocoded_unassigned_addresses.csv")

geocoded_results |>
  select(congregation_name, num_unassigned, cities, address) |>
  kable(
    col.names = c("Congregation", "# Properties", "Cities", "Geocoded Address")
  ) |>
  kable_styling(bootstrap_options = c("striped", "hover"))

# Create a lookup table
address_lookup <- geocoded_results %>%
  select(congregation_name, sample_lat, sample_lon, geocoded_address = address)

# Join back to property_profile to fill in missing addresses
property_profile_updated <- property_profile %>%
  left_join(address_lookup, by = c("congregation_name", "lat" = "sample_lat", "lon" = "sample_lon")) %>%
  mutate(
    sadd = if_else(is.na(sadd) | sadd == "", geocoded_address, sadd)
  ) %>%
  select(-geocoded_address)

write_csv(property_profile_updated, "data/output/property_profile_updated.csv")

cat(sprintf("\nCongregations with unassigned/missing addresses: %d\n", 
            nrow(unassigned_addresses)))
cat(sprintf("Total properties affected: %d\n", 
            sum(unassigned_addresses$num_unassigned)))

property_profile %>%
  filter(congregation_name %in% c("St Johns Episcopal Church", 
                                  "St Pauls Church", 
                                  "St Stephens Church")) %>%
  select(congregation_name, sadd, lat, lon) %>%
  print(n = 20)

property_profile %>%
  filter(is.na(sadd) | sadd == "") %>%
  count(congregation_name)

address_lookup_simple <- geocoded_results %>%
  select(congregation_name, geocoded_address = address)

# Update properties that have "UNASSIGNED" in the address
property_profile_updated <- property_profile %>%
  left_join(address_lookup_simple, by = "congregation_name") %>%
  mutate(
    sadd = case_when(
      # Replace if address contains "UNASSIGNED" and we have a geocoded address
      str_detect(sadd, "UNASSIGNED") & !is.na(geocoded_address) ~ geocoded_address,
      TRUE ~ sadd
    )
  ) %>%
  select(-geocoded_address)

# Check the results
property_profile_updated %>%
  filter(congregation_name %in% c("St Johns Episcopal Church", 
                                  "St Pauls Church", 
                                  "St Stephens Church")) %>%
  select(congregation_name, sadd, lat, lon) %>%
  print(n = 20)

# Simple summary showing just the shared attributes
duplicate_summary <- property_profile_updated %>%
  group_by(congregation_name, sadd, attendance_2023, plate_pledge_2023) %>%
  filter(n() > 1) %>%
  summarise(
    n_entries = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(n_entries))

# Display
duplicate_summary %>%
  kable(
    col.names = c("Congregation", "Address", "Attendance 2023", "Plate/Pledge 2023", "# Entries"),
    digits = 0,
    format.args = list(big.mark = ",")
  ) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

# Then separately show the data quality issue for one example
# Pick one congregation to illustrate the problem
property_profile_updated %>%
  filter(congregation_name == "St Johns Episcopal Church",  # or whichever has most duplicates
         sadd == "902 MAIN ST, WEST POINT 23181") %>%  # adjust address
  select(pid, lan_val, rgisacre, deed_acres, zon) %>%
  kable(
    col.names = c("PID", "Land Value", "GIS Acreage", "Deed Acreage", "Zoning"),
    caption = "Example: Multiple entries with different property data for the same congregation/address",
    digits = 2,
    format.args = list(big.mark = ",")
  ) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

## 3B: Shared Addresses (Different congregations at same location) ----
shared_by_address <- property_profile_updated %>%
  filter(!is.na(sadd) & sadd != "") %>%
  group_by(sadd, scity) %>%
  filter(n_distinct(congregation_name) > 1) %>%
  arrange(sadd, congregation_name) %>%
  select(sadd, scity, congregation_name, attendance_2023, uid, pid) %>%
  ungroup()


cat(sprintf("\nDifferent congregations at same address: %d addresses\n", 
            n_distinct(shared_by_address$sadd)))
cat(sprintf("Congregations affected: %d\n", 
            n_distinct(shared_by_address$congregation_name)))

## 3C: Shared Locations (by coordinates) ----
shared_by_location <- property_profile %>%
  filter(!is.na(lat) & !is.na(lon)) %>%
  mutate(
    lat_round = round(lat, 4),
    lon_round = round(lon, 4)
  ) %>%
  group_by(lat_round, lon_round) %>%
  filter(n_distinct(congregation_name) > 1) %>%
  arrange(lat_round, lon_round, congregation_name) %>%
  select(lat_round, lon_round, sadd, scity, congregation_name, attendance_2023) %>%
  ungroup()

if(nrow(shared_by_location) > 0) {
  cat(sprintf("Different congregations at same coordinates: %d locations\n", 
              n_distinct(paste(shared_by_location$lat_round, shared_by_location$lon_round))))
}


# SECTION 4: MULTI-PROPERTY CONGREGATION ANALYSIS ----


cat("\n=== SECTION 4: MULTI-PROPERTY CONGREGATIONS ===\n")

## Count properties per congregation ----
properties_per_congregation <- property_profile %>%
  group_by(congregation_name) %>%
  summarise(
    property_count = n(),
    unique_addresses = n_distinct(sadd, na.rm = TRUE),
    unique_coords = n_distinct(paste(lat, lon), na.rm = TRUE),
    unique_pids = n_distinct(pid, na.rm = TRUE),
    avg_data_completeness = mean(data_completeness, na.rm = TRUE),
    min_data_completeness = min(data_completeness, na.rm = TRUE),
    max_data_completeness = max(data_completeness, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(property_count))

cat(sprintf("Congregations with multiple properties: %d\n", 
            sum(properties_per_congregation$property_count > 1)))

## Identify "problem congregations" (multiple properties + incomplete data) ----
problem_congregations <- properties_per_congregation %>%
  filter(property_count > 1 & min_data_completeness < 6)

cat(sprintf("Problem congregations (multiple properties + incomplete data): %d\n", 
            nrow(problem_congregations)))

## Check for potential duplicates (same congregation, same coordinates) ----
potential_duplicates <- property_profile %>%
  filter(!is.na(lat) & !is.na(lon)) %>%
  group_by(congregation_name, lat, lon) %>%
  filter(n() > 1) %>%
  arrange(congregation_name, lat, lon) %>%
  select(congregation_name, uid, pid, sadd, scity, lat, lon, 
         rgisacre, lan_val, data_completeness) %>%
  ungroup()

if(nrow(potential_duplicates) > 0) {
  cat(sprintf("Potential duplicate records (same location): %d properties\n", 
              nrow(potential_duplicates)))
}


# SECTION 5: COMPREHENSIVE PROBLEM IDENTIFICATION ----


cat("\n=== SECTION 5: COMPREHENSIVE PROBLEM MATRIX ===\n")

## Combine all problem flags into one comprehensive view ----
full_comparison <- problem_congregations %>%
  full_join(
    shared_by_address %>% 
      group_by(congregation_name) %>%
      summarise(
        has_shared_address = TRUE,
        shared_addresses = paste(unique(sadd), collapse = " | "),
        shared_cities = paste(unique(scity), collapse = ", "),
        num_shared_addresses = n_distinct(sadd),
        other_congregations_at_address = n_distinct(congregation_name) - 1,
        .groups = 'drop'
      ),
    by = "congregation_name"
  ) %>%
  full_join(
    unassigned_addresses %>%
      select(congregation_name, num_unassigned, unassigned_addresses, 
             cities, has_coords, sample_lat, sample_lon),
    by = "congregation_name"
  ) %>%
  left_join(
    # Add location info from property_profile
    property_profile %>%
      group_by(congregation_name) %>%
      summarise(
        scity = first(scity[!is.na(scity)]),
        scounty = first(scounty[!is.na(scounty)]),
        szip = first(szip[!is.na(szip)]),
        lat = first(lat[!is.na(lat)]),
        lon = first(lon[!is.na(lon)]),
        .groups = 'drop'
      ),
    by = "congregation_name"
  ) %>%
  mutate(
    has_shared_address = replace_na(has_shared_address, FALSE),
    has_unassigned = !is.na(num_unassigned) & num_unassigned > 0,
    shared_addresses = replace_na(shared_addresses, "None"),
    num_shared_addresses = replace_na(num_shared_addresses, 0),
    num_unassigned = replace_na(num_unassigned, 0),
    # Create comprehensive address status flag
    address_status = case_when(
      has_shared_address & has_unassigned ~ "Both shared AND unassigned",
      has_shared_address ~ "Has shared addresses",
      has_unassigned ~ "Has unassigned addresses",
      TRUE ~ "All addresses unique and assigned"
    )
  ) %>%
  arrange(desc(has_unassigned), desc(has_shared_address), desc(property_count)) %>%
  select(congregation_name, property_count, address_status,
         scity, scounty, szip, lat, lon,  
         has_shared_address, shared_addresses, shared_cities, num_shared_addresses,
         has_unassigned, num_unassigned, unassigned_addresses,
         unique_addresses, unique_coords, avg_data_completeness,
         min_data_completeness, max_data_completeness)

## Break out mutually exclusive categories ----
shared_only <- full_comparison %>%
  filter(has_shared_address == TRUE & has_unassigned == FALSE)

unassigned_only <- full_comparison %>%
  filter(has_unassigned == TRUE & has_shared_address == FALSE)

both_issues <- full_comparison %>%
  filter(has_shared_address == TRUE & has_unassigned == TRUE)

cat("\nProblem categories:\n")
cat(sprintf("  • Shared addresses only: %d\n", nrow(shared_only)))
cat(sprintf("  • Unassigned addresses only: %d\n", nrow(unassigned_only)))
cat(sprintf("  • Both issues: %d (HIGHEST PRIORITY)\n", nrow(both_issues)))


# SECTION 6: CONGREGATION-LEVEL SUMMARY ----


cat("\n=== SECTION 6: CONGREGATION-LEVEL SUMMARY ===\n")

## Roll up all properties to congregation level ----
congregation_summary <- property_profile %>%
  group_by(congregation_name) %>%
  summarise(
    num_parcels = n(),
    total_acres = sum(rgisacre, na.rm = TRUE),
    total_land_value = sum(lan_val, na.rm = TRUE),
    has_any_qct = any(qct == 1, na.rm = TRUE),
    has_any_dda = any(dda == 1, na.rm = TRUE),
    any_developable = any(developable == TRUE, na.rm = TRUE),
    all_developable = all(developable == TRUE, na.rm = TRUE),
    best_data_completeness = max(data_completeness, na.rm = TRUE),
    worst_data_completeness = min(data_completeness, na.rm = TRUE),
    addresses = paste(unique(sadd[!is.na(sadd)]), collapse = "; "),
    cities = paste(unique(scity[!is.na(scity)]), collapse = ", "),
    attendance_2023 = first(attendance_2023),
    members_2023 = first(members_2023),
    .groups = 'drop'
  ) %>%
  arrange(desc(num_parcels), desc(total_land_value))

cat(sprintf("Total unique congregations: %d\n", nrow(congregation_summary)))
cat(sprintf("Congregations with multiple parcels: %d\n", 
            sum(congregation_summary$num_parcels > 1)))

## Create action plan ----
action_needed <- congregation_summary %>%
  mutate(
    recommended_action = case_when(
      num_parcels == 1 & worst_data_completeness < 6 ~ 
        "Single parcel - Fill missing data",
      num_parcels > 1 & best_data_completeness == 6 & worst_data_completeness == 6 ~ 
        "Multiple parcels - All complete",
      num_parcels > 1 & worst_data_completeness < 6 ~ 
        "Multiple parcels - Fill missing data",
      num_parcels > 1 & best_data_completeness == 6 & worst_data_completeness < 6 ~ 
        "Multiple parcels - Some complete, verify others",
      TRUE ~ "Review needed"
    )
  ) %>%
  select(congregation_name, num_parcels, best_data_completeness, 
         worst_data_completeness, recommended_action, total_acres, 
         total_land_value, addresses)


# SECTION 7: EXPORTS ----


cat("\n=== SECTION 7: EXPORTING RESULTS ===\n")

# Create output directory if needed
dir.create("data/output", recursive = TRUE, showWarnings = FALSE)

# Export data quality issues
write_csv(incomplete_properties, "data/output/incomplete_properties.csv")
cat("✓ Exported incomplete_properties.csv\n")

# Export address issues
write_csv(unassigned_addresses, "data/output/unassigned_addresses.csv")
cat("✓ Exported unassigned_addresses.csv\n")

write_csv(shared_by_address, "data/output/shared_addresses.csv")
cat("✓ Exported shared_addresses.csv\n")

if(nrow(shared_by_location) > 0) {
  write_csv(shared_by_location, "data/output/shared_locations.csv")
  cat("✓ Exported shared_locations.csv\n")
}

# Export multi-property analysis
write_csv(properties_per_congregation, "data/output/properties_per_congregation.csv")
cat("✓ Exported properties_per_congregation.csv\n")

if(nrow(potential_duplicates) > 0) {
  write_csv(potential_duplicates, "data/output/potential_duplicates.csv")
  cat("✓ Exported potential_duplicates.csv\n")
}

# Export comprehensive problem matrix
write_csv(full_comparison, "data/output/problem_matrix_comprehensive.csv")
cat("✓ Exported problem_matrix_comprehensive.csv\n")

if(nrow(shared_only) > 0) {
  write_csv(shared_only, "data/output/problem_shared_only.csv")
  cat("✓ Exported problem_shared_only.csv\n")
}

if(nrow(unassigned_only) > 0) {
  write_csv(unassigned_only, "data/output/problem_unassigned_only.csv")
  cat("✓ Exported problem_unassigned_only.csv\n")
}

if(nrow(both_issues) > 0) {
  write_csv(both_issues, "data/output/problem_both_issues.csv")
  cat("✓ Exported problem_both_issues.csv (HIGHEST PRIORITY)\n")
}

# Export congregation summaries
write_csv(congregation_summary, "data/output/congregation_summary.csv")
cat("✓ Exported congregation_summary.csv\n")

write_csv(action_needed, "data/output/action_plan.csv")
cat("✓ Exported action_plan.csv\n")


# SECTION 8: FINAL SUMMARY ----


cat("\n")
cat("===========================================\n")
cat("PROBLEM PROPERTIES ANALYSIS COMPLETE\n")
cat("===========================================\n")
cat(sprintf("Total properties analyzed: %d\n", nrow(property_profile)))
cat(sprintf("Properties with data issues: %d\n", nrow(incomplete_properties)))
cat(sprintf("Congregations with address issues: %d\n", 
            nrow(unassigned_addresses) + n_distinct(shared_by_address$congregation_name)))
cat(sprintf("Congregations needing immediate attention: %d\n", nrow(both_issues)))
cat("\nAll outputs saved to data/output/\n")
cat("===========================================\n")