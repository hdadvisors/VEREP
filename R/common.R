# _common.R
library(tidyverse)
library(shiny)
library(bslib)
library(DT)
library(ggplot2)
library(plotly)
library(rsconnect)
library(kableExtra)
library(leaflet)
library(leaflet.extras)

property_profile <- read_csv("data/output/verep_full_analysis.csv", show_col_types = FALSE)
attendance_context <- read_csv("data/output/verep_attendance_summary.csv", show_col_types = FALSE)
opportunity_matrix <- read_csv("data/output/verep_opportunity_matrix.csv", show_col_types = FALSE)
leadership_report <- read_csv("data/output/verep_top_opportunities.csv", show_col_types = FALSE)
data_followup <- read_csv("data/output/verep_data_needed.csv", show_col_types = FALSE)
incomplete_properties <- read_csv("data/output/incomplete_properties.csv", show_col_types = FALSE)
unassigned_addresses <- read_csv("data/output/unassigned_addresses.csv", show_col_types = FALSE)
shared_addresses <- read_csv("data/output/shared_addresses.csv", show_col_types = FALSE)
problem_matrix <- read_csv("data/output/problem_matrix_comprehensive.csv", show_col_types = FALSE)

# Create scored_parcels with development scores
scored_parcels <- property_profile %>%
  mutate(
    area_acres = rgisacre,
    development_score = case_when(
      development_potential == "High Potential" ~ 85,
      development_potential == "Moderate Potential" ~ 70,
      development_potential == "Low Potential - Constraints" ~ 40,
      development_potential == "Insufficient Data" ~ 30,
      TRUE ~ 20
    ),
    development_tier = development_potential,
    assessed_value = lan_val
  ) %>%
  filter(!is.na(area_acres), area_acres > 0.1, !is.na(lat) & !is.na(lon))

# Create top_10
top_10 <- scored_parcels %>%
  filter(development_potential %in% c("High Potential", "Moderate Potential")) %>%
  arrange(desc(assessed_value)) %>%
  slice_head(n = 10) %>%
  mutate(rank = row_number())