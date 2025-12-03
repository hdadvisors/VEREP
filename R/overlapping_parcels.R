# ==============================================================================
# Overlapping Parcels Analysis
# Purpose: Identify and analyze congregations with multiple parcels at same address
# Note: PIDs were created to handle overlapping parcels from parcel data issues
# ==============================================================================

# Load libraries ----
library(tidyverse)
library(knitr)
library(kableExtra)

# Load data ----
property_profile <- read_csv("data/output/property_profile_updated.csv", show_col_types = FALSE)

# ==============================================================================
# SECTION 1: Identify Overlapping Parcels
# ==============================================================================

# Summary: Congregations with multiple parcels at same address
overlapping_parcels_summary <- property_profile_updated %>%
  group_by(congregation_name, sadd) %>%
  filter(n() > 1) %>%
  summarise(
    n_parcels = n(),
    attendance_2023 = first(attendance_2023),
    plate_pledge_2023 = first(plate_pledge_2023),
    total_land_value = sum(lan_val, na.rm = TRUE),
    total_gis_acreage = sum(rgisacre, na.rm = TRUE),
    total_deed_acreage = sum(deed_acres, na.rm = TRUE),
    pids = paste(pid, collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(desc(n_parcels))

# Detailed view: All individual parcels that overlap
overlapping_parcels_detail <- property_profile_updated %>%
  group_by(congregation_name, sadd) %>%
  filter(n() > 1) %>%
  arrange(congregation_name, sadd, pid) %>%
  select(congregation_name, sadd, scity, pid, uid, 
         lan_val, rgisacre, deed_acres, zon, zon_desc,
         lat, lon, qct, fema_fz) %>%
  ungroup()

# ==============================================================================
# SECTION 2: Statistics
# ==============================================================================

# Calculate key statistics
total_congregations_with_overlap <- n_distinct(overlapping_parcels_summary$congregation_name)
total_overlapping_parcels <- sum(overlapping_parcels_summary$n_parcels)
max_parcels_per_congregation <- max(overlapping_parcels_summary$n_parcels)

cat("Overlapping Parcels Statistics:\n")
cat("  Congregations with multiple parcels:", total_congregations_with_overlap, "\n")
cat("  Total overlapping parcels:", total_overlapping_parcels, "\n")
cat("  Maximum parcels at one address:", max_parcels_per_congregation, "\n\n")

# ==============================================================================
# SECTION 3: Export Results
# ==============================================================================

# Export summary
write_csv(overlapping_parcels_summary, "data/output/overlapping_parcels_summary.csv")

# Export detailed view
write_csv(overlapping_parcels_detail, "data/output/overlapping_parcels_detail.csv")

cat("Files exported:\n")
cat("  - overlapping_parcels_summary.csv\n")
cat("  - overlapping_parcels_detail.csv\n")

# ==============================================================================
# SECTION 4: Example Analysis - Congregation with Most Parcels
# ==============================================================================

# Find congregation with most overlapping parcels
top_congregation <- overlapping_parcels_summary %>%
  slice_max(n_parcels, n = 1)

cat("\nCongregation with most overlapping parcels:\n")
cat("  Name:", top_congregation$congregation_name, "\n")
cat("  Address:", top_congregation$sadd, "\n")
cat("  Number of parcels:", top_congregation$n_parcels, "\n")

# Show detailed breakdown for this congregation
top_congregation_detail <- overlapping_parcels_detail %>%
  filter(congregation_name == top_congregation$congregation_name,
         sadd == top_congregation$sadd) %>%
  select(pid, lan_val, rgisacre, deed_acres, zon_desc)

print(top_congregation_detail)

# ==============================================================================
# SECTION 5: Distribution Analysis
# ==============================================================================

# Distribution of number of parcels per congregation
parcel_distribution <- overlapping_parcels_summary %>%
  count(n_parcels, name = "n_congregations") %>%
  arrange(n_parcels)

cat("\nDistribution of overlapping parcels:\n")
print(parcel_distribution)

# ===========