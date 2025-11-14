# Load required libraries
library(tidyverse)
library(readr)
library(scales)
library(devtools)
devtools::install_github("hdadvisors/hdatools", force = TRUE)
library(hdatools)
library(ggplot2)
library(dplyr)

# Load the RDS file
property_profile <- readRDS("data/output/property_profile.rds")

# Identify the "problem children" - properties with multiple missing fields
problem_properties <- property_profile %>%
  filter(data_completeness < 6) %>%
  arrange(data_completeness) %>%
  select(congregation_name, data_completeness, missing_land_value, missing_acreage, 
         missing_wetland, missing_flood, missing_qct, missing_zoning)

cat(sprintf("\nProperties with Most Missing Data (completeness score < 6):\n"))
if(nrow(problem_properties) > 0) {
  # Show top 10 worst cases
  top_problems <- head(problem_properties, 10)
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
} else {
  cat("  None - all properties have complete data!\n")
}
cat("===========================================\n")

# Export key findings
# For leadership - high-level opportunities
leadership_report <- property_profile %>%
  filter(development_potential %in% c("High Potential", "Moderate Potential")) %>%
  select(congregation_name, attendance_2023, lan_val, rgisacre, 
         development_potential, qct, scity, sstate)

write_csv(leadership_report, "data/output/verep_top_opportunities.csv")

# For data team - properties needing follow-up (the "problem children")
data_followup <- property_profile %>%
  filter(data_completeness < 6) %>%
  select(congregation_name, data_completeness, sadd, scity, sstate, szip, missing_land_value, 
         missing_acreage, missing_wetland, missing_flood, missing_qct, missing_zoning,
         scity, sstate) %>%
  arrange(data_completeness)

write_csv(data_followup, "data/output/verep_data_needed.csv")

# Full detailed analysis
write_csv(property_profile, "data/output/verep_full_analysis.csv")

# Export summary statistics
write_csv(attendance_context, "data/output/verep_attendance_summary.csv")
write_csv(opportunity_matrix, "data/output/verep_opportunity_matrix.csv")

cat("\n=== EXPORTS COMPLETE ===\n")
cat("Files saved to output/ directory:\n")
cat(sprintf("  • verep_top_opportunities.csv (%d properties)\n", nrow(leadership_report)))
cat(sprintf("  • verep_data_needed.csv (%d properties with missing data)\n", nrow(data_followup)))
cat(sprintf("  • verep_full_analysis.csv (%d properties)\n", nrow(property_profile)))
cat("  • verep_attendance_summary.csv\n")
cat("  • verep_opportunity_matrix.csv\n")
cat("\nAnalysis complete!\n")

# ========================================
# INVESTIGATE POTENTIAL DUPLICATE/MULTIPLE PROPERTIES
# ========================================

# 1. Count properties per congregation
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

cat("\n=== CONGREGATIONS WITH MULTIPLE PROPERTIES ===\n")
print(head(properties_per_congregation, 20))

# 2. Focus on congregations with incomplete data AND multiple properties
problem_congregations <- properties_per_congregation %>%
  filter(property_count > 1 & min_data_completeness < 6)

cat(sprintf("\n%d congregations have multiple properties with incomplete data\n", 
            nrow(problem_congregations)))

# First, identify congregations with unassigned addresses
unassigned_addresses <- property_profile %>%
  filter(is.na(sadd) | sadd == "" | grepl("UNASSIGNED", sadd, ignore.case = TRUE)) %>%
  group_by(congregation_name) %>%
  summarise(
    has_unassigned = TRUE,
    num_unassigned = n(),
    unassigned_addresses = paste(unique(sadd[!is.na(sadd) & sadd != ""]), collapse = " | "),
    .groups = 'drop'
  )

cat("\n=== CONGREGATIONS WITH UNASSIGNED ADDRESSES ===\n")
cat(sprintf("Found %d congregations with unassigned/missing addresses\n\n", 
            nrow(unassigned_addresses)))
print(unassigned_addresses)

# Enhanced comparison with BOTH shared and unassigned addresses
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
    unassigned_addresses,
    by = "congregation_name"
  ) %>%
  mutate(
    has_shared_address = replace_na(has_shared_address, FALSE),
    has_unassigned = replace_na(has_unassigned, FALSE),
    shared_addresses = replace_na(shared_addresses, "None"),
    num_shared_addresses = replace_na(num_shared_addresses, 0),
    num_unassigned = replace_na(num_unassigned, 0),
    # Create a combined address status flag
    address_status = case_when(
      has_shared_address & has_unassigned ~ "Both shared AND unassigned addresses",
      has_shared_address ~ "Has shared addresses",
      has_unassigned ~ "Has unassigned addresses",
      TRUE ~ "All addresses unique and assigned"
    )
  ) %>%
  arrange(desc(has_unassigned), desc(has_shared_address), desc(property_count)) %>%
  select(congregation_name, property_count, address_status,
         has_shared_address, shared_addresses, shared_cities, num_shared_addresses,
         has_unassigned, num_unassigned, unassigned_addresses,
         unique_addresses, unique_coords, avg_data_completeness,
         min_data_completeness, max_data_completeness)

cat("\n=== FULL COMPARISON WITH SHARED & UNASSIGNED ADDRESSES ===\n")
print(full_comparison)

# Export
write_csv(full_comparison, "data/output/review1.csv")

# Break out specific problem categories (mutually exclusive)
shared_only <- full_comparison %>%
  filter(has_shared_address == TRUE & has_unassigned == FALSE)

unassigned_only <- full_comparison %>%
  filter(has_unassigned == TRUE & has_shared_address == FALSE)

both_issues <- full_comparison %>%
  filter(has_shared_address == TRUE & has_unassigned == TRUE)

# Display summaries
cat("\n=== SUMMARY BY ADDRESS STATUS ===\n")
cat(sprintf("Problem congregations with shared addresses: %d\n", nrow(shared_only)))
cat(sprintf("Problem congregations with unassigned addresses: %d\n", nrow(unassigned_only)))
cat(sprintf("Problem congregations with BOTH issues: %d\n", nrow(both_issues)))

# Export separate files for each category
if(nrow(shared_only) > 0) {
  cat("\n=== CONGREGATIONS WITH SHARED ADDRESSES ===\n")
  print(shared_only)
  write_csv(shared_only, "data/output/problem_congregations_with_shared_addresses.csv")
}

if(nrow(unassigned_only) > 0) {
  cat("\n=== CONGREGATIONS WITH UNASSIGNED ADDRESSES ===\n")
  print(unassigned_only)
  write_csv(unassigned_only, "data/output/problem_congregations_with_unassigned_addresses.csv")
}

if(nrow(both_issues) > 0) {
  cat("\n=== CONGREGATIONS WITH BOTH SHARED AND UNASSIGNED ===\n")
  print(both_issues)
  write_csv(both_issues, "data/output/problem_congregations_with_both_issues.csv")
}

cat("\n=== EXPORTS COMPLETE ===\n")


# 3. Detailed view of specific congregation's properties
inspect_congregation <- function(congregation_name) {
  cat(sprintf("\n=== DETAILED VIEW: %s ===\n", congregation_name))
  
  congregation_properties <- property_profile %>%
    filter(congregation_name == congregation_name) %>%
    select(uid, pid, sadd, scity, szip, lat, lon, 
           rgisacre, lan_val, data_completeness,
           missing_land_value, missing_acreage, missing_wetland, 
           missing_flood, missing_qct, missing_zoning)
  
  print(congregation_properties)
  
  # Check if coordinates are truly different
  if(nrow(congregation_properties) > 1) {
    coords_check <- congregation_properties %>%
      select(uid, pid, lat, lon) %>%
      mutate(
        lat_diff = abs(lat - lag(lat)),
        lon_diff = abs(lon - lag(lon))
      )
    
    cat("\nCoordinate differences:\n")
    print(coords_check)
    
    # Flag likely duplicates (same coordinates)
    same_location <- coords_check %>%
      filter(lat_diff < 0.0001 | is.na(lat_diff)) %>%
      nrow()
    
    if(same_location > 1) {
      cat(sprintf("\n⚠️  WARNING: %d properties appear to be at the same location\n", same_location))
    }
  }
}

# Inspect a few examples
if(nrow(problem_congregations) > 0) {
  cat("\n=== INSPECTING TOP 3 PROBLEM CONGREGATIONS ===\n")
  for(i in 1:min(3, nrow(problem_congregations))) {
    inspect_congregation(problem_congregations$congregation_name[i])
  }
}

# 4. Identify likely true duplicates (same congregation + same coordinates)
potential_duplicates <- property_profile %>%
  filter(!is.na(lat) & !is.na(lon)) %>%
  group_by(congregation_name, lat, lon) %>%
  filter(n() > 1) %>%
  arrange(congregation_name, lat, lon) %>%
  select(congregation_name, uid, pid, sadd, scity, lat, lon, 
         rgisacre, lan_val, data_completeness)

if(nrow(potential_duplicates) > 0) {
  cat(sprintf("\n=== POTENTIAL DUPLICATES (Same Location) ===\n"))
  cat(sprintf("Found %d properties that share coordinates with another property\n\n", 
              nrow(potential_duplicates)))
  print(potential_duplicates)
} else {
  cat("\nNo duplicate coordinates found - all properties are at unique locations\n")
}

# 5. Identify adjacent parcels (likely legitimately separate)
adjacent_parcels <- property_profile %>%
  filter(!is.na(lat) & !is.na(lon)) %>%
  group_by(congregation_name) %>%
  filter(n() > 1) %>%
  arrange(congregation_name, sadd) %>%
  select(congregation_name, uid, pid, sadd, scity, lat, lon, 
         rgisacre, lan_val, data_completeness)

cat(sprintf("\n=== ADJACENT/MULTIPLE PARCELS (Unique Coordinates) ===\n"))
cat(sprintf("Found %d properties from %d congregations with multiple parcels\n\n",
            nrow(adjacent_parcels),
            n_distinct(adjacent_parcels$congregation_name)))

# Show examples
if(nrow(adjacent_parcels) > 0) {
  cat("Examples of congregations with multiple distinct parcels:\n")
  print(head(adjacent_parcels, 15))
}

# 6. Create a cleaner summary: one row per congregation
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
    # Keep key attendance data
    attendance_2023 = first(attendance_2023),
    members_2023 = first(members_2023),
    .groups = 'drop'
  ) %>%
  arrange(desc(num_parcels), desc(total_land_value))

cat("\n=== CONGREGATION-LEVEL SUMMARY (All Parcels Combined) ===\n")
print(head(congregation_summary, 20))

# Export this cleaner view
write_csv(congregation_summary, "data/output/verep_congregation_summary.csv")

# 7. Decision matrix: What to do with each congregation
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

cat("\n=== RECOMMENDED ACTIONS ===\n")
print(action_needed %>% filter(worst_data_completeness < 6))

write_csv(action_needed, "data/output/verep_action_plan.csv")

# 8. Specific check for properties with "UNASSIGNED" addresses
unassigned_properties <- property_profile %>%
  filter(grepl("UNASSIGNED", sadd, ignore.case = TRUE)) %>%
  select(congregation_name, uid, pid, sadd, scity, lat, lon, 
         rgisacre, lan_val, data_completeness) %>%
  arrange(congregation_name)

if(nrow(unassigned_properties) > 0) {
  cat(sprintf("\n=== PROPERTIES WITH 'UNASSIGNED' ADDRESSES ===\n"))
  cat(sprintf("Found %d properties with unassigned addresses\n\n", 
              nrow(unassigned_properties)))
  print(unassigned_properties)
}

cat("\n=== EXPORTS COMPLETE ===\n")
cat("New analysis files saved:\n")
cat("  • verep_congregation_summary.csv - One row per congregation\n")
cat("  • verep_action_plan.csv - Recommended next steps\n")

# ========================================
# FIND DIFFERENT CONGREGATIONS AT SAME ADDRESS - SIMPLIFIED
# ========================================

# Method 1: Check by street address
shared_by_address <- property_profile %>%
  filter(!is.na(sadd) & sadd != "") %>%
  group_by(sadd) %>%
  filter(n_distinct(congregation_name) > 1) %>%
  arrange(sadd, congregation_name) %>%
  select(sadd, scity, congregation_name, attendance_2023, uid, pid) %>%
  ungroup()

cat("\n=== DIFFERENT CONGREGATIONS AT SAME ADDRESS ===\n")
if(nrow(shared_by_address) > 0) {
  cat(sprintf("Found %d properties with different congregations at same address\n\n", 
              nrow(shared_by_address)))
  print(shared_by_address)
  write_csv(shared_by_address, "data/output/shared_addresses.csv")
} else {
  cat("No shared addresses found\n")
}

# Method 2: Check by coordinates (location)
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

cat("\n=== DIFFERENT CONGREGATIONS AT SAME LOCATION ===\n")
if(nrow(shared_by_location) > 0) {
  cat(sprintf("Found %d properties with different congregations at same location\n\n", 
              nrow(shared_by_location)))
  print(shared_by_location)
  write_csv(shared_by_location, "data/output/shared_locations.csv")
} else {
  cat("No shared locations found\n")
}

cat("\nDone!\n")

cat("\n=== SUMMARY ===\n")
cat(sprintf("Congregations with problems: %d\n", nrow(problem_congregations)))
cat(sprintf("Congregations sharing addresses: %d\n", 
            n_distinct(shared_by_address$congregation_name)))
cat(sprintf("Congregations in BOTH lists: %d\n", 
            sum(problem_congregations$congregation_name %in% shared_by_address$congregation_name)))


# Enhanced comparison with addresses included
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
  mutate(
    has_shared_address = replace_na(has_shared_address, FALSE),
    shared_addresses = replace_na(shared_addresses, "None"),
    num_shared_addresses = replace_na(num_shared_addresses, 0)
  ) %>%
  arrange(desc(has_shared_address), desc(property_count)) %>%
  select(congregation_name, property_count, has_shared_address, 
         shared_addresses, shared_cities, num_shared_addresses,
         unique_addresses, unique_coords, avg_data_completeness,
         min_data_completeness, max_data_completeness)

cat("\n=== FULL COMPARISON WITH ADDRESSES ===\n")
print(full_comparison)

# Show just the ones WITH shared addresses for easier review
shared_only <- full_comparison %>%
  filter(has_shared_address == TRUE)

if(nrow(shared_only) > 0) {
  cat("\n=== PROBLEM CONGREGATIONS THAT SHARE ADDRESSES ===\n")
  print(shared_only)
  write_csv(shared_only, "data/output/problem_congregations_with_shared_addresses.csv")
}