# VEREP Property Analysis for Properties with <30 Sunday Attendees
# Analysis focuses on: Land Value, Zoning/Density, Acreage, Environmental Limitations, QCT Status

# Load required libraries
library(tidyverse)
library(readr)
library(scales)
library(devtools)
devtools::install_github("hdadvisors/hdatools", force = TRUE)
library(hdatools)
library(ggplot2)
library(dplyr)

# Read the data
verep_data <- read_csv("data/verep040725.csv")
codebook <- read_csv("data/CGS_VEREP_AppendixD_Codebook.xlsx.csv")

cat("Columns in verep_data CSV:\n")
print(names(verep_data))

# ========================================
# STEP 1: LOAD AND JOIN ATTENDANCE DATA
# ========================================
# Read congregation attendance data
congregation_data <- read_csv("data/congregation.csv")

# Prepare attendance data with most recent year (2023), average, AND historical trends
attendance_data <- congregation_data %>%
  select(
    quickref,
    congregation_name = name,
    short_name,
    # Current data
    attendance_2023 = f2023_sunday_attendance,
    attendance_avg,
    members_2023 = f2023_members_this_year,
    plate_pledge_2023 = f2023_plate_and_pledge,
    # Historical data for trend analysis
    attendance_2014 = f2014_sunday_attendance,
    members_2014 = f2014_members_this_year,
    plate_pledge_2014 = f2014_plate_and_pledge,
    attendance_pctinc,
    dio_reg
  ) %>%
  mutate(
    # Calculate 10-year trends (2014-2023)
    pct_change_attendance = case_when(
      is.na(attendance_2014) | attendance_2014 == 0 ~ NA_real_,
      TRUE ~ ((attendance_2023 - attendance_2014) / attendance_2014) * 100
    ),
    pct_change_pledge = case_when(
      is.na(plate_pledge_2014) | plate_pledge_2014 == 0 ~ NA_real_,
      TRUE ~ ((plate_pledge_2023 - plate_pledge_2014) / plate_pledge_2014) * 100
    )
  )

# Join property data with attendance data
verep_data <- attendance_data %>%
  left_join(
    verep_data, 
    by = c("quickref" = "congr_name"),  # FIX: Use "congr_name" from verep CSV
    suffix = c("_att", "_prop")  # ADD: Prevents duplicate column names
  )

# Display join results
cat("=== JOIN RESULTS ===\n")
cat(sprintf("Total properties: %d\n", nrow(verep_data)))
cat(sprintf("Properties matched to congregation: %d\n", 
            sum(!is.na(verep_data$attendance_2023))))
cat(sprintf("Properties with attendance data: %d\n\n", 
            sum(!is.na(verep_data$attendance_2023) & verep_data$attendance_2023 > 0)))

# ========================================
# STEP 2: FILTER FOR <30 SUNDAY ATTENDEES
# ========================================
# Filter for properties with <30 Sunday attendees (using 2023 data)
analysis_subset <- verep_data %>%
  filter(
    !is.na(attendance_2023) &  # Has attendance data
      attendance_2023 < 30        # Less than 30 attendees
  )

cat("=== FILTERING RESULTS ===\n")
cat(sprintf("Properties with <30 Sunday attendees: %d\n", nrow(analysis_subset)))
cat(sprintf("Unique congregations represented: %d\n\n", 
            n_distinct(analysis_subset$congregation_name[!is.na(analysis_subset$congregation_name)])))

# Show congregations included
congregations_under30 <- analysis_subset %>%
  filter(!is.na(congregation_name)) %>%
  distinct(congregation_name, attendance_2023) %>%
  arrange(attendance_2023)

cat("=== CONGREGATIONS WITH <30 ATTENDEES ===\n")
print(congregations_under30)

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
  theme_hda() 

print(land_value_plot)

# ========================================
# METRIC 2: ZONING/DENSITY ANALYSIS
# ========================================

# Create a lookup table from complete records
zoning_lookup <- analysis_subset %>%
  filter(!is.na(zon) & !is.na(zon_desc) & !is.na(zon_type)) %>%
  # For each zoning code, get the most common description and type
  group_by(zon) %>%
  summarise(
    zon_desc_lookup = names(sort(table(zon_desc), decreasing = TRUE))[1],
    zon_type_lookup = names(sort(table(zon_type), decreasing = TRUE))[1],
    .groups = 'drop'
  )

# View the lookup table
print("=== ZONING LOOKUP TABLE ===")
print(zoning_lookup)

# Join the lookup back and fill in missing values
analysis_subset_clean <- analysis_subset %>%
  left_join(zoning_lookup, by = "zon") %>%
  mutate(
    zon_desc = coalesce(zon_desc, zon_desc_lookup),
    zon_type = coalesce(zon_type, zon_type_lookup)
  ) %>%
  select(-zon_desc_lookup, -zon_type_lookup)  # Remove the lookup columns

# Check how many were filled in
cat("\n=== FILL RESULTS ===\n")
cat(sprintf("Before: %d properties with zoning description\n", 
            sum(!is.na(analysis_subset$zon_desc))))
cat(sprintf("After: %d properties with zoning description\n", 
            sum(!is.na(analysis_subset_clean$zon_desc))))
cat(sprintf("Before: %d properties with zoning type\n", 
            sum(!is.na(analysis_subset$zon_type))))
cat(sprintf("After: %d properties with zoning type\n", 
            sum(!is.na(analysis_subset_clean$zon_type))))

# Now use the cleaned data for your analysis
zoning_summary <- analysis_subset_clean %>%
  filter(!is.na(zon)) %>%
  count(zon, zon_desc, sort = TRUE) %>%
  mutate(percentage = n / sum(n) * 100)

zoning_type_summary <- analysis_subset_clean %>%
  filter(!is.na(zon_type)) %>%
  count(zon_type, sort = TRUE) %>%
  mutate(percentage = n / sum(n) * 100)

print("\n=== ZONING TYPE SUMMARY (CLEANED) ===")
print(zoning_type_summary)


# ========================================
# METRIC 3: ACREAGE ANALYSIS
# ========================================
acreage_summary <- analysis_subset %>%
  summarise(
    total_properties = n(),  # Add this to show total
    properties_with_acreage = sum(!is.na(rgisacre)),
    properties_missing_acreage = sum(is.na(rgisacre)),  # Add this to count NAs
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

# Show which properties are missing acreage data
missing_acreage_properties <- analysis_subset %>%
  filter(is.na(rgisacre)) %>%
  select(congregation_name, short_name, attendance_2023, deed_acres, rgisacre)

cat(sprintf("\nNote: %d properties excluded due to missing acreage data:\n", 
            sum(is.na(analysis_subset$rgisacre))))
print(missing_acreage_properties)

# ========================================
# METRIC 4: ENVIRONMENTAL LIMITATIONS
# ========================================
environmental_summary <- analysis_subset %>%
  summarise(
    # Total properties
    total_properties = n(),
    
    # Wetlands
    properties_with_wetlands = sum(wet_perc > 0, na.rm = TRUE),
    avg_wetland_coverage = mean(wet_perc, na.rm = TRUE),
    significant_wetlands = sum(wet_perc > 10, na.rm = TRUE),
    missing_wetland_data = sum(is.na(wet_perc)),  
    
    # Flood zones
    in_flood_zone = sum(!is.na(fema_fz) & fema_fz != "" & fema_fz != "X", na.rm = TRUE),
    in_high_risk_flood = sum(fema_fz %in% c("A", "AE", "AO", "AH", "V", "VE"), na.rm = TRUE),
    missing_flood_zone_data = sum(is.na(fema_fz) | fema_fz == ""),
    
    # FEMA Risk Rating
    high_fema_risk = sum(fema_nri %in% c("Very High", "Relatively High"), na.rm = TRUE),
    missing_fema_risk_data = sum(is.na(fema_nri))  
  )

print("\n=== ENVIRONMENTAL CONDITIONS SUMMARY ===")
print(environmental_summary)

# Show which properties are missing environmental data
missing_environmental <- analysis_subset %>%
  filter(is.na(wet_perc) | is.na(fema_fz) | fema_fz == "" | is.na(fema_nri)) %>%
  select(congregation_name, short_name, wet_perc, fema_fz, fema_nri)

if(nrow(missing_environmental) > 0) {
  cat("\n=== PROPERTIES WITH MISSING ENVIRONMENTAL DATA ===\n")
  print(missing_environmental)
}

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
    missing_qct = sum(is.na(qct)),
    
    # Also check DDA status
    in_dda = sum(dda == 1, na.rm = TRUE),
    dda_percentage = sum(dda == 1, na.rm = TRUE) / n() * 100
  )

print("\n=== LIHTC QCT & DDA SUMMARY ===")
print(qct_summary)

# Show which properties are missing qct data
missing_qct <- analysis_subset %>%
  filter(is.na(qct) | is.na(dda)) %>%  # FIXED: Check for QCT and DDA, not environmental vars
  select(congregation_name, short_name, qct, dda)  # FIXED: Select QCT/DDA columns

if(nrow(missing_qct) > 0) {  # FIXED: Check missing_qct, not missing_environmental
  cat("\n=== PROPERTIES WITH MISSING QCT/DDA DATA ===\n")  # FIXED: Updated message
  print(missing_qct)
}

# ========================================
# COMPREHENSIVE PROPERTY PROFILE
# ========================================
property_profile <- analysis_subset %>%
  mutate(
    # Flag missing crucial data
    missing_land_value = is.na(lan_val),
    missing_acreage = is.na(rgisacre),
    missing_wetland = is.na(wet_perc),
    missing_flood = is.na(fema_fz) | fema_fz == "",
    missing_qct = is.na(qct),
    missing_zoning = is.na(zon) | is.na(zon_desc),
    
    # Calculate data completeness score (0-6 points)
    data_completeness = 6 - (missing_land_value + missing_acreage + 
                               missing_wetland + missing_flood + 
                               missing_qct + missing_zoning),
    
    # Environmental constraints (with NA handling)
    has_environmental_constraint = case_when(
      is.na(wet_perc) & is.na(fema_fz) ~ NA,  # Can't determine if both missing
      (!is.na(wet_perc) & wet_perc > 10) ~ TRUE,
      (!is.na(fema_fz) & fema_fz %in% c("A", "AE", "AO", "AH", "V", "VE")) ~ TRUE,
      TRUE ~ FALSE
    ),
    
    # Developable flag (requires key data)
    developable = case_when(
      missing_acreage | is.na(has_environmental_constraint) ~ NA,  # Can't determine
      rgisacre >= 1 & !has_environmental_constraint ~ TRUE,
      TRUE ~ FALSE
    ),
    
    # Development potential category
    development_potential = case_when(
      data_completeness < 4 ~ "Insufficient Data",
      developable == TRUE & !missing_land_value & lan_val >= 100000 ~ "High Potential",
      developable == TRUE ~ "Moderate Potential",
      developable == FALSE ~ "Low Potential - Constraints",
      is.na(developable) ~ "Unknown - Missing Data"
    ),
    
    # Calculate land value per acre
    land_value_per_acre = if_else(rgisacre > 0, lan_val / rgisacre, NA_real_)
  ) %>%
  select(
    uid, pid, congregation_name, 
    attendance_2023, members_2023, attendance_avg,
    attendance_2014, pct_change_attendance,  # ADD THESE
    plate_pledge_2023, plate_pledge_2014, pct_change_pledge,  # ADD THESE
    lan_val, land_value_per_acre, rgisacre, deed_acres,
    zon, zon_desc, zon_type,
    qct, dda, 
    wet_perc, fema_fz, fema_nri,
    has_environmental_constraint, developable, development_potential,
    data_completeness, missing_land_value, missing_acreage, 
    missing_wetland, missing_flood, missing_qct, missing_zoning,
    sadd, scity, scounty, sstate, szip,
    lat, lon,
    walk_idx, church, parking, open_space, cemetery, school, residence
  ) %>%
  arrange(desc(data_completeness), desc(developable), desc(lan_val))

# Save as RDS
saveRDS(property_profile, "data/output/property_profile.rds")

print("\n=== TOP DEVELOPMENT OPPORTUNITIES (First 10) ===")
print(head(property_profile %>% 
             select(congregation_name, attendance_2023, lan_val, rgisacre, 
                    development_potential, data_completeness, scity), 10))

# Summary of development potential
dev_potential_summary <- property_profile %>%
  count(development_potential) %>%
  mutate(percentage = n / sum(n) * 100)

print("\n=== DEVELOPMENT POTENTIAL SUMMARY ===")
print(dev_potential_summary)

# Properties with insufficient data
incomplete_data <- property_profile %>%
  filter(data_completeness < 6) %>%
  select(congregation_name, data_completeness, missing_land_value, 
         missing_acreage, missing_wetland, missing_flood, missing_qct, missing_zoning)

if(nrow(incomplete_data) > 0) {
  cat("\n=== PROPERTIES WITH INCOMPLETE DATA ===\n")
  print(incomplete_data)
}

## Exports 
dir.create("output", showWarnings = FALSE)

# For leadership - high-level opportunities
leadership_report <- property_profile %>%
  filter(development_potential %in% c("High Potential", "Moderate Potential")) %>%
  select(congregation_name, attendance_2023, lan_val, rgisacre, 
         development_potential, qct, scity, sstate)

write_csv(leadership_report, "output/verep_top_opportunities.csv")

# For data team - properties needing follow-up
data_followup <- property_profile %>%
  filter(data_completeness < 6) %>%
  select(congregation_name, data_completeness, missing_land_value, 
         missing_acreage, missing_wetland, missing_flood, missing_qct, missing_zoning,
         scity, sstate)

write_csv(data_followup, "output/verep_data_needed.csv")

# Full detailed analysis
write_csv(property_profile, "output/verep_full_analysis.csv")

# ========================================
# OPPORTUNITY MATRIX
# ========================================
opportunity_matrix <- property_profile %>%
  summarise(
    high_opportunity = sum(qct == 1 & rgisacre >= 1 & !has_environmental_constraint, na.rm = TRUE),
    medium_opportunity = sum((qct == 1 | dda == 1) & rgisacre >= 0.5 & rgisacre < 1, na.rm = TRUE),
    constrained_properties = sum(has_environmental_constraint == TRUE, na.rm = TRUE),
    small_parcels = sum(rgisacre < 0.5, na.rm = TRUE),
    missing_data = sum(is.na(qct) | is.na(rgisacre) | is.na(has_environmental_constraint)),
    other_properties = n() - sum(
      qct == 1 & rgisacre >= 1 & !has_environmental_constraint |
        (qct == 1 | dda == 1) & rgisacre >= 0.5 & rgisacre < 1 |
        has_environmental_constraint == TRUE |
        rgisacre < 0.5 |
        is.na(qct) | is.na(rgisacre) | is.na(has_environmental_constraint),
      na.rm = TRUE
    ),
    total_parcels = n()
  )

print("\n=== OPPORTUNITY MATRIX ===")
print(opportunity_matrix)

# Show properties with missing key data
missing_key_data <- property_profile %>%
  filter(is.na(qct) | is.na(rgisacre) | is.na(has_environmental_constraint)) %>%
  select(congregation_name, rgisacre, qct, dda, has_environmental_constraint, 
         missing_acreage, missing_qct, missing_wetland, missing_flood, data_completeness)

if(nrow(missing_key_data) > 0) {
  cat("\n=== PROPERTIES WITH MISSING KEY DATA ===\n")
  print(missing_key_data)
}


# See what the "other" properties are that aren't fitting neatly in all categories
other_properties <- property_profile %>%
  filter(
    !(qct == 1 & rgisacre >= 1 & !has_environmental_constraint) &
      !((qct == 1 | dda == 1) & rgisacre >= 0.5 & rgisacre < 1) &
      !(has_environmental_constraint == TRUE) &
      !(rgisacre < 0.5) &
      !is.na(qct) & !is.na(rgisacre) & !is.na(has_environmental_constraint)
  ) %>%
  select(congregation_name, rgisacre, qct, dda, has_environmental_constraint, development_potential)

cat("\n=== 'OTHER' PROPERTIES (not fitting main categories) ===\n")
print(other_properties)

# ========================================
# ATTENDANCE CONTEXT ANALYSIS
# ========================================
attendance_context <- analysis_subset %>%  # FIX: Use analysis_subset, not analysis_subset_clean
  summarise(
    total_properties = n(),
    
    # 2023 Attendance
    mean_attendance_2023 = mean(attendance_2023, na.rm = TRUE),
    median_attendance_2023 = median(attendance_2023, na.rm = TRUE),
    zero_attendance_2023 = sum(attendance_2023 == 0, na.rm = TRUE),
    attendance_1_10_2023 = sum(attendance_2023 > 0 & attendance_2023 <= 10, na.rm = TRUE),
    attendance_11_20_2023 = sum(attendance_2023 > 10 & attendance_2023 <= 20, na.rm = TRUE),
    attendance_21_29_2023 = sum(attendance_2023 > 20 & attendance_2023 < 30, na.rm = TRUE),
    
    # 2014 Attendance (for comparison)
    mean_attendance_2014 = mean(attendance_2014, na.rm = TRUE),
    median_attendance_2014 = median(attendance_2014, na.rm = TRUE),
    zero_attendance_2014 = sum(attendance_2014 == 0, na.rm = TRUE),
    attendance_1_10_2014 = sum(attendance_2014 > 0 & attendance_2014 <= 10, na.rm = TRUE),
    attendance_11_20_2014 = sum(attendance_2014 > 10 & attendance_2014 <= 20, na.rm = TRUE),
    attendance_21_29_2014 = sum(attendance_2014 > 20 & attendance_2014 < 30, na.rm = TRUE),
    
    # 10-year change in attendance
    avg_pct_change_attendance = mean(pct_change_attendance, na.rm = TRUE),
    median_pct_change_attendance = median(pct_change_attendance, na.rm = TRUE),
    
    # Members
    avg_members_2023 = mean(members_2023, na.rm = TRUE),
    avg_members_2014 = mean(members_2014, na.rm = TRUE),  # CHANGED THIS
    
    # Plate & Pledge
    avg_plate_pledge_2023 = mean(plate_pledge_2023, na.rm = TRUE),
    avg_plate_pledge_2014 = mean(plate_pledge_2014, na.rm = TRUE),
    avg_pct_change_pledge = mean(pct_change_pledge, na.rm = TRUE),
    
    # Missing data counts
    missing_attendance_2023 = sum(is.na(attendance_2023)),
    missing_attendance_2014 = sum(is.na(attendance_2014)),
    missing_members_2023 = sum(is.na(members_2023)),
    missing_members_2014 = sum(is.na(members_2014)),  # CHANGED THIS
    missing_plate_pledge_2023 = sum(is.na(plate_pledge_2023)),
    missing_plate_pledge_2014 = sum(is.na(plate_pledge_2014))
  )

cat("\n=== ATTENDANCE PROFILE (2014 vs 2023) ===\n")
print(attendance_context)


# Create summary report
cat("\n===========================================\n")
cat("EXECUTIVE SUMMARY: PROPERTIES WITH <30 SUNDAY ATTENDEES\n")
cat("===========================================\n")
cat(sprintf("Total Properties Analyzed: %d\n", nrow(analysis_subset)))
cat(sprintf("Unique Congregations: %d\n", 
            n_distinct(analysis_subset$congr_name[!is.na(analysis_subset$congr_name)])))
cat(sprintf("Average 2023 Attendance: %.1f\n", attendance_context$mean_attendance_2023))
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
cat("\n--- DATA QUALITY ISSUES ---\n")
cat(sprintf("Properties with Missing Data: %d (%.1f%%)\n", 
            sum(property_profile$data_completeness < 6),
            sum(property_profile$data_completeness < 6) / nrow(property_profile) * 100))

