# VEREP: Virginia Episcopal Real Estate Portfolio Analysis

## Project Overview

This project provides a comprehensive development opportunity analysis for the Virginia Episcopal Real Estate Portfolio (VEREP). It identifies and evaluates underutilized church properties for potential development, focusing on properties that could generate revenue for congregations while supporting affordable housing goals.

The analysis was conducted by **HDAdvisors** in conjunction with case study projects for VEREP (Virginia Episcopal Real Estate Partners).

## Key Features

- **Development Scoring System**: Multi-dimensional scoring algorithm evaluating properties across 6 criteria
- **Interactive Shiny Dashboard**: Web-based exploration of property data with mapping and filtering
- **Quarto Book Report**: Comprehensive static report with methodology, analysis, and property profiles
- **ADU Placement Tool**: Interactive tool for visualizing accessory dwelling unit placement on properties

## Project Structure

```
VEREP/
├── R/
│   ├── common.R                 # Shared data loading and processing
│   ├── app.R                    # Shiny dashboard application
│   ├── overlapping_parcels.R    # Parcel analysis utilities
│   └── problem_properties_coding.R  # Data quality issue identification
├── data/
│   └── output/                  # Processed CSV data files
│       ├── verep_full_analysis.csv
│       ├── verep_attendance_summary.csv
│       ├── verep_opportunity_matrix.csv
│       ├── verep_top_opportunities.csv
│       ├── verep_data_needed.csv
│       ├── incomplete_properties.csv
│       ├── unassigned_addresses.csv
│       ├── shared_addresses.csv
│       └── problem_matrix_comprehensive.csv
├── *.qmd                        # Quarto book chapters
├── _quarto.yml                  # Quarto configuration
├── hda-styles.css               # Custom styling
└── README.md                    # This file
```

## Data Sources

| Dataset | Description |
|---------|-------------|
| `verep_full_analysis.csv` | Complete property profiles with development scores |
| `verep_attendance_summary.csv` | Congregation attendance trends (2014-2023) |
| `verep_opportunity_matrix.csv` | Development opportunity classifications |
| `verep_top_opportunities.csv` | Leadership report of top development candidates |
| `incomplete_properties.csv` | Properties with missing data fields |
| `unassigned_addresses.csv` | Properties without valid street addresses |
| `shared_addresses.csv` | Properties sharing addresses (potential data issues) |
| `problem_matrix_comprehensive.csv` | Comprehensive data quality issue tracking |

## Development Scoring Methodology

Properties are scored across six weighted dimensions:

| Dimension | Weight | Description |
|-----------|--------|-------------|
| Current Use | 25% | Underutilized properties (parking, open space) score highest |
| Property Size | 20% | Optimal range: 0.5-5 acres |
| Location Quality | 20% | Walkability index and transit access |
| Financial Need | 15% | Congregation pledge trends (declining = higher priority) |
| Market Potential | 10% | Area median income, LIHTC/QCT eligibility |
| Zoning | 10% | Development-friendly classifications |

### Constraint Penalties

- Flood zones (100-year): -40 points
- Wetlands >25%: -35 points
- Easements: -30 points
- Historic districts: -25 points
- Wetlands 10-25%: -20 points

### Development Tiers

| Tier | Score Range | Classification |
|------|-------------|----------------|
| Tier 1 | 75-100 | High Priority |
| Tier 2 | 60-74 | Strong Potential |
| Tier 3 | 45-59 | Moderate Potential |
| Tier 4 | 30-44 | Limited Potential |
| Tier 5 | <30 | Not Recommended |

## Installation & Setup

### Prerequisites

- R (>= 4.0)
- RStudio (recommended)
- Quarto (>= 1.3)

### Required R Packages

```r
install.packages(c(
  "tidyverse",
  "shiny",
  "shinydashboard",
  "bslib",
  "DT",
  "ggplot2",
  "plotly",
  "kableExtra",
  "leaflet",
  "leaflet.extras",
  "scales",
  "officer",
  "flextable",
  "sf",
  "writexl",
  "tidygeocoder"
))
```

## Usage

### Run the Shiny Dashboard

```r
shiny::runApp("R/app.R")
```

The dashboard provides:
- Executive summary with key metrics
- Interactive property map with filtering
- Sortable property rankings table
- ADU placement visualization tool
- PowerPoint and Excel export functionality

### Render the Quarto Report

```bash
quarto render
```

Or in RStudio: Build > Render Book

The rendered book will be output to the `_book/` directory.


## License

Proprietary - HDAdvisors

## Contact

HDAdvisors  
[Contact information]

---

*Analysis completed for VEREP (Virginia Episcopal Real Estate Partners)*


Data Cleaning: Joining VEREP Property Data with Congregation Attendance
The Challenge
The VEREP property dataset and the congregation attendance dataset use different naming conventions for the same churches:

VEREP: "St Lukes Church (Simeon) (Charlottesville)" — includes location in parentheses
Congregation: "St Lukes Church (Simeon)" — partial parenthetical, no city

Additionally, common church names like "Grace Church," "Trinity Church," and "St Johns Church" appear in multiple cities, creating ambiguity when matching by name alone.
The Solution
We join the datasets using two standardized fields: cleaned name and cleaned city.
Step 1: Standardize names in both datasets

Convert to lowercase
Remove all parenthetical content (e.g., location identifiers)
Trim whitespace

This transforms both "St Lukes Church (Simeon) (Charlottesville)" and "St Lukes Church (Simeon)" into "st lukes church".
Step 2: Standardize city names

Convert to lowercase
Trim whitespace

Step 3: Join on both fields
Using a left_join on clean_name and clean_city ensures:

Each VEREP property matches at most one congregation
Properties without a congregation match are retained (with NA attendance)
No duplicate rows are created from many-to-many matches

Result
Of 790 VEREP properties, 421 matched to a congregation with attendance data. After filtering to congregations with fewer than 30 Sunday attendees (2023), the final analysis dataset contains 104 unique properties.
Lessons Learned

Apply identical transformations to both sides of a join — cleaning only one dataset produces zero matches
Join on multiple fields when names are ambiguous — name-only joins create duplicates when common names exist in multiple locations
Use left_join to preserve the primary dataset — other join types can introduce unexpected rows