# VEREP Property Analysis for Properties with <30 Sunday Attendees
# Analysis focuses on: Land Value, Zoning/Density, Acreage, Environmental Limitations, QCT Status

# Load required libraries
library(tidyverse)
library(readr)
library(scales)

# ========================================
# CORRECTED: CLEAN BOTH DATASETS THE SAME WAY
# ========================================

# Re-read data fresh to avoid column conflicts
verep_data <- read_csv("data/verep040725.csv")
congregation_data <- read_csv("data/congregation.csv")

# Clean VEREP names - remove ALL parenthetical content
verep_data <- verep_data %>%
  mutate(
    clean_name = str_to_lower(congr_name),
    clean_name = str_replace_all(clean_name, "\\s*\\([^)]+\\)", ""),
    clean_name = str_trim(clean_name),
    clean_city = str_to_lower(str_trim(scity))
  )

# Clean congregation names - ALSO remove parentheses
congregation_data <- congregation_data %>%
  mutate(
    clean_name = str_to_lower(name),
    clean_name = str_replace_all(clean_name, "\\s*\\([^)]+\\)", ""),  # <-- Added this line
    clean_name = str_trim(clean_name),
    clean_city = str_to_lower(str_trim(city))
  )

# Prepare attendance data
attendance_data <- congregation_data %>%
  select(
    clean_name,
    clean_city,
    short_name,
    attendance_2023 = f2023_sunday_attendance,
    attendance_avg,
    members_2023 = f2023_members_this_year,
    plate_pledge_2023 = f2023_plate_and_pledge,
    attendance_pctinc,
    dio_reg
  )

# Join on both cleaned name and city
verep_data <- verep_data %>%
  left_join(attendance_data, by = c("clean_name", "clean_city"))

# Check results
cat("=== JOIN RESULTS (NAME + CITY, BOTH CLEANED) ===\n")
cat(sprintf("Total properties: %d\n", nrow(verep_data)))
cat(sprintf("Properties matched to congregation: %d\n", 
            sum(!is.na(verep_data$attendance_2023))))
cat(sprintf("Properties with attendance data: %d\n\n", 
            sum(!is.na(verep_data$attendance_2023) & verep_data$attendance_2023 > 0)))

# Now filter for <30 attendance
analysis_subset <- verep_data %>%
  filter(
    !is.na(attendance_2023) &
      attendance_2023 < 30
  )

cat(sprintf("Properties with <30 Sunday attendees: %d\n", nrow(analysis_subset)))

# ========================================
# METRIC 1: LAND VALUE ANALYSIS
# ========================================
land_value_summary <- analysis_subset %>%
  summarise(
    total_properties = n(),
    properties_with_value = sum(!is.na(lan_val)),
    mean_land_value = mean(lan_val, na.rm = TRUE),
    median_land_value = median(lan_val, na.rm = TRUE),
    min_land_value = min(lan_val, na.rm = TRUE),
    max_land_value = max(lan_val, na.rm = TRUE),
    total_land_value = sum(lan_val, na.rm = TRUE)
  )

print("=== LAND VALUE SUMMARY ===")
print(land_value_summary)

# Land value distribution
land_value_plot <- analysis_subset %>%
  filter(!is.na(lan_val)) %>%
  ggplot(aes(x = lan_val)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
  scale_x_continuous(labels = dollar_format()) +
  labs(
    title = "Distribution of Land Values",
    subtitle = "Church Properties",
    x = "Land Value",
    y = "Count"
  ) +
  theme_minimal()

# ========================================
# METRIC 2: ZONING/DENSITY ANALYSIS
# ========================================
zoning_summary <- analysis_subset %>%
  filter(!is.na(zon)) %>%
  count(zon, zon_desc, sort = TRUE) %>%
  mutate(percentage = n / sum(n) * 100)

print("\n=== ZONING SUMMARY (Top 10) ===")
print(head(zoning_summary, 10))

# Zoning type summary
zoning_type_summary <- analysis_subset %>%
  filter(!is.na(zon_type)) %>%
  count(zon_type, sort = TRUE)

print("\n=== ZONING TYPE SUMMARY ===")
print(zoning_type_summary)

# ========================================
# METRIC 3: ACREAGE ANALYSIS
# ========================================
acreage_summary <- analysis_subset %>%
  summarise(
    properties_with_acreage = sum(!is.na(rgisacre)),
    mean_acres = mean(rgisacre, na.rm = TRUE),
    median_acres = median(rgisacre, na.rm = TRUE),
    total_acres = sum(rgisacre, na.rm = TRUE),
    min_acres = min(rgisacre, na.rm = TRUE),
    max_acres = max(rgisacre, na.rm = TRUE)
  )

print("\n=== ACREAGE SUMMARY ===")
print(acreage_summary)

# Acreage categories
acreage_categories <- analysis_subset %>%
  filter(!is.na(rgisacre)) %>%
  mutate(
    acreage_category = case_when(
      rgisacre < 0.5 ~ "< 0.5 acres",
      rgisacre < 1 ~ "0.5-1 acres",
      rgisacre < 2 ~ "1-2 acres",
      rgisacre < 5 ~ "2-5 acres",
      rgisacre < 10 ~ "5-10 acres",
      TRUE ~ "10+ acres"
    )
  ) %>%
  count(acreage_category) %>%
  mutate(percentage = n / sum(n) * 100)

print("\n=== ACREAGE DISTRIBUTION ===")
print(acreage_categories)

# ========================================
# METRIC 4: ENVIRONMENTAL LIMITATIONS
# ========================================
environmental_summary <- analysis_subset %>%
  summarise(
    # Wetlands
    properties_with_wetlands = sum(wet_perc > 0, na.rm = TRUE),
    avg_wetland_coverage = mean(wet_perc, na.rm = TRUE),
    significant_wetlands = sum(wet_perc > 10, na.rm = TRUE),
    
    # Flood zones
    in_flood_zone = sum(!is.na(fema_fz) & fema_fz != "" & fema_fz != "X", na.rm = TRUE),
    in_high_risk_flood = sum(fema_fz %in% c("A", "AE", "AO", "AH", "V", "VE"), na.rm = TRUE),
    
    # FEMA Risk Rating
    high_fema_risk = sum(fema_nri %in% c("Very High", "Relatively High"), na.rm = TRUE)
  )

print("\n=== ENVIRONMENTAL LIMITATIONS SUMMARY ===")
print(environmental_summary)

# Flood zone breakdown
flood_zone_summary <- analysis_subset %>%
  filter(!is.na(fema_fz) & fema_fz != "") %>%
  count(fema_fz, sort = TRUE) %>%
  mutate(percentage = n / sum(n) * 100)

print("\n=== FLOOD ZONE BREAKDOWN ===")
print(flood_zone_summary)

# ========================================
# METRIC 5: LIHTC QCT ZONE PRESENCE
# ========================================
qct_summary <- analysis_subset %>%
  summarise(
    total_properties = n(),
    in_qct = sum(qct == 1, na.rm = TRUE),
    not_in_qct = sum(qct == 0, na.rm = TRUE),
    qct_percentage = sum(qct == 1, na.rm = TRUE) / n() * 100,
    
    # Also check DDA status
    in_dda = sum(dda == 1, na.rm = TRUE),
    dda_percentage = sum(dda == 1, na.rm = TRUE) / n() * 100
  )

print("\n=== LIHTC QCT & DDA SUMMARY ===")
print(qct_summary)

# ========================================
# COMPREHENSIVE PROPERTY PROFILE
# ========================================
property_profile <- analysis_subset %>%
  filter(!is.na(lan_val)) %>%
  mutate(
    has_environmental_constraint = (wet_perc > 10) | 
      (fema_fz %in% c("A", "AE", "AO", "AH", "V", "VE")),
    developable = (qct == 1) & (rgisacre >= 1) & (!has_environmental_constraint),
    
    # Calculate land value per acre
    land_value_per_acre = if_else(rgisacre > 0, lan_val / rgisacre, NA_real_)
  ) %>%
  select(
    uid, pid, congr_name, 
    attendance_2023, members_2023, attendance_avg,
    lan_val, land_value_per_acre, rgisacre, deed_acres,
    zon, zon_desc, zon_type,
    qct, dda, 
    wet_perc, fema_fz, fema_nri,
    has_environmental_constraint, developable,
    sadd, scity, scounty, sstate, szip,
    lat, lon
  ) %>%
  arrange(desc(developable), desc(lan_val))

print("\n=== TOP DEVELOPMENT OPPORTUNITIES (First 10) ===")
print(head(property_profile %>% 
             select(congr_name, attendance_2023, lan_val, rgisacre, 
                    qct, developable, scity), 10))

# Export filtered results
write_csv(property_profile, "verep_analysis_results.csv")

# Properties with <30 attendance that are MISSING land value data
properties_no_land_value <- analysis_subset %>%
  filter(is.na(lan_val)) %>%
  select(
    uid, pid, congr_name,
    attendance_2023, members_2023, attendance_avg,
    lan_val, rgisacre, deed_acres,
    zon, zon_desc, zon_type,
    qct, dda,
    sadd, scity, scounty, sstate, szip,
    lat, lon
  ) %>%
  arrange(congr_name)

# Summary
cat("=== PROPERTIES WITH <30 ATTENDANCE AND NO LAND VALUE ===\n")
cat(sprintf("Total properties missing land value: %d\n", nrow(properties_no_land_value)))
cat(sprintf("Out of total <30 attendance properties: %d\n", nrow(analysis_subset)))
cat(sprintf("Percentage missing land value: %.1f%%\n\n", 
            nrow(properties_no_land_value) / nrow(analysis_subset) * 100))

# Show the properties
print(properties_no_land_value)

# Export
write_csv(properties_no_land_value, "properties_under30_no_land_value.csv")
cat("\nExported to 'properties_under30_no_land_value.csv'\n")

# ========================================
# OPPORTUNITY MATRIX
# ========================================
opportunity_matrix <- property_profile %>%
  summarise(
    high_opportunity = sum(qct == 1 & rgisacre >= 1 & !has_environmental_constraint, na.rm = TRUE),
    medium_opportunity = sum((qct == 1 | dda == 1) & rgisacre >= 0.5 & rgisacre < 1, na.rm = TRUE),
    constrained_properties = sum(has_environmental_constraint == TRUE, na.rm = TRUE),
    small_parcels = sum(rgisacre < 0.5, na.rm = TRUE)
  )

print("\n=== DEVELOPMENT OPPORTUNITY MATRIX ===")
print(opportunity_matrix)

# ========================================
# ATTENDANCE CONTEXT ANALYSIS
# ========================================
attendance_context <- analysis_subset %>%
  filter(!is.na(attendance_2023)) %>%
  summarise(
    mean_attendance = mean(attendance_2023, na.rm = TRUE),
    median_attendance = median(attendance_2023, na.rm = TRUE),
    zero_attendance = sum(attendance_2023 == 0, na.rm = TRUE),
    attendance_1_10 = sum(attendance_2023 > 0 & attendance_2023 <= 10, na.rm = TRUE),
    attendance_11_20 = sum(attendance_2023 > 10 & attendance_2023 <= 20, na.rm = TRUE),
    attendance_21_29 = sum(attendance_2023 > 20 & attendance_2023 < 30, na.rm = TRUE),
    avg_members = mean(members_2023, na.rm = TRUE),
    avg_plate_pledge = mean(plate_pledge_2023, na.rm = TRUE)
  )

cat("\n=== ATTENDANCE PROFILE ===\n")
print(attendance_context)

# Create summary report
cat("\n===========================================\n")
cat("EXECUTIVE SUMMARY: PROPERTIES WITH <30 SUNDAY ATTENDEES\n")
cat("===========================================\n")
cat(sprintf("Total Properties Analyzed: %d\n", nrow(analysis_subset)))
cat(sprintf("Unique Congregations: %d\n", 
            n_distinct(analysis_subset$congr_name[!is.na(analysis_subset$congr_name)])))
cat(sprintf("Average 2023 Attendance: %.1f\n", attendance_context$mean_attendance))
cat("\n--- DEVELOPMENT METRICS ---\n")
cat(sprintf("Properties in QCT Zones: %d (%.1f%%)\n", 
            qct_summary$in_qct, qct_summary$qct_percentage))
cat(sprintf("Average Land Value: %s\n", 
            dollar(land_value_summary$mean_land_value)))
cat(sprintf("Total Land Value: %s\n", 
            dollar(land_value_summary$total_land_value)))
cat(sprintf("Median Acreage: %.2f acres\n", 
            acreage_summary$median_acres))
cat(sprintf("Total Acreage: %.2f acres\n", 
            acreage_summary$total_acres))
cat("\n--- CONSTRAINTS ---\n")
cat(sprintf("Properties with Environmental Constraints: %d (%.1f%%)\n", 
            sum(property_profile$has_environmental_constraint, na.rm = TRUE),
            sum(property_profile$has_environmental_constraint, na.rm = TRUE) / nrow(property_profile) * 100))
cat(sprintf("Properties in Flood Zones: %d\n", 
            environmental_summary$in_flood_zone))
cat(sprintf("Properties with Significant Wetlands (>10%%): %d\n", 
            environmental_summary$significant_wetlands))
cat("\n--- DEVELOPMENT POTENTIAL ---\n")
cat(sprintf("High Opportunity Properties: %d\n", opportunity_matrix$high_opportunity))
cat(sprintf("  (QCT + ≥1 acre + no major constraints)\n"))
cat(sprintf("Medium Opportunity Properties: %d\n", opportunity_matrix$medium_opportunity))
cat(sprintf("  (QCT/DDA + 0.5-1 acre)\n"))
cat("===========================================\n")

# Export key findings
cat("\nExporting results to 'verep_analysis_results.csv'\n")
cat("Analysis complete!\n")