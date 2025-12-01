# VEREP Development Analysis Shiny Dashboard
# File: app.R
# Save this in your verep-clean folder

library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(DT)
library(scales)
library(officer)
library(flextable)
library(sf)

# Load and process data (using your existing code)
parcels_raw <- read_csv("data/verep040725.csv")
congregations_raw <- read_csv("data/congregation.csv")

# Process congregations
congregations <- congregations_raw %>%
  mutate(
    pct_change_attendance = ((f2023_sunday_attendance - f2014_sunday_attendance) / 
                               f2014_sunday_attendance) * 100,
    pct_change_members = ((f2023_members_this_year - f2014_members_this_year) / 
                            f2014_members_this_year) * 100,
    pct_change_pledge = ((f2023_plate_and_pledge - f2014_plate_and_pledge) / 
                           f2014_plate_and_pledge) * 100,
    avg_attendance = attendance_avg,
    avg_members = member_avg,
    avg_pledge = platepledge_avg,
    financial_need = case_when(
      pct_change_pledge < -20 ~ 3,
      pct_change_pledge < 0 ~ 2,
      pct_change_pledge < 10 ~ 1,
      TRUE ~ 0
    ),
    size_category = case_when(
      avg_attendance < 25 ~ "Very Small",
      avg_attendance < 50 ~ "Small",
      avg_attendance < 100 ~ "Medium",
      avg_attendance < 200 ~ "Large",
      TRUE ~ "Very Large"
    )
  ) %>%
  mutate(across(starts_with("pct_change"), ~replace_na(., 0)),
         across(starts_with("avg_"), ~replace_na(., 0)))

# Process parcels
df <- parcels_raw %>%
  mutate(
    area_acres = case_when(
      !is.na(rgisacre) ~ rgisacre,
      !is.na(rgissqft) ~ rgissqft / 43560,
      !is.na(deed_acres) ~ deed_acres,
      TRUE ~ NA_real_
    ),
    use_church = church == 1,
    use_cemetery = cemetery == 1,
    use_school = school == 1,
    use_parking = parking == 1,
    use_open_space = open_space == 1,
    use_residence = residence == 1,
    flood_zone = case_when(
      is.na(fema_fz) | fema_fz == "X" ~ "None",
      TRUE ~ fema_fz
    ),
    wetlands_pct = replace_na(wet_perc, 0),
    has_easement = !is.na(easement) & easement != "",
    in_historic_district = !is.na(hist_dist) & hist_dist != "",
    walkability_score = replace_na(walk_idx, 0),
    transit_access_score = case_when(
      trans_acc >= 3 ~ 100,
      trans_acc == 2 ~ 60,
      trans_acc == 1 ~ 30,
      TRUE ~ 0
    ),
    zoning_favorable = case_when(
      str_detect(zon_desc, regex("mixed|commercial|residential", ignore_case = TRUE)) ~ TRUE,
      TRUE ~ FALSE
    ),
    median_income = medinc,
    poverty_rate = pov_prc,
    qualified_census_tract = !is.na(qct) & qct != 0,
    year_built = case_when(
      yr_blt_num > 1700 & yr_blt_num < 2025 ~ yr_blt_num,
      TRUE ~ NA_real_
    ),
    assessed_value = par_val,
    site_address = sadd,
    site_city = scity,
    site_county = scounty
  ) %>%
  filter(!is.na(area_acres), area_acres > 0.1, !is.na(lat) & !is.na(lon))

# Join data
df_joined <- df %>%
  left_join(
    congregations %>% select(name, short_name, pct_change_attendance, pct_change_members, 
                             pct_change_pledge, avg_attendance, avg_members, avg_pledge,
                             financial_need, size_category),
    by = c("congr_name" = "short_name")
  ) %>%
  mutate(
    has_congregation = !is.na(name),
    pct_change_pledge = case_when(
      !has_congregation & use_church ~ 0,
      TRUE ~ pct_change_pledge
    ),
    financial_need = case_when(
      !has_congregation & use_church ~ 1,
      TRUE ~ financial_need
    )
  )

# Calculate scores
scored_parcels <- df_joined %>%
  mutate(
    size_score = case_when(
      area_acres < 0.25 ~ 20, area_acres < 0.5 ~ 50, area_acres < 1 ~ 70,
      area_acres < 2 ~ 90, area_acres < 5 ~ 100, area_acres < 10 ~ 85,
      TRUE ~ 70
    ),
    use_score = case_when(
      use_parking ~ 95, use_open_space ~ 90, use_cemetery ~ 5,
      use_church & !use_parking & !use_open_space ~ 30,
      use_residence ~ 40, use_school ~ 35, TRUE ~ 60
    ),
    location_score = (
      (walkability_score / max(walkability_score, na.rm = TRUE) * 60) +
        (transit_access_score * 0.4)
    ),
    location_score = pmin(100, pmax(0, location_score)),
    financial_score = case_when(
      is.na(financial_need) ~ 30, financial_need == 3 ~ 100,
      financial_need == 2 ~ 70, financial_need == 1 ~ 40, TRUE ~ 20
    ),
    market_score = case_when(
      is.na(median_income) ~ 50, median_income > 100000 ~ 90,
      median_income > 75000 ~ 75, median_income > 50000 ~ 60, TRUE ~ 40
    ),
    market_score = if_else(qualified_census_tract, market_score + 15, market_score),
    market_score = pmin(100, market_score),
    zoning_score = if_else(zoning_favorable, 80, 40),
    constraint_penalty = case_when(
      flood_zone != "None" ~ -40, wetlands_pct > 25 ~ -35,
      has_easement ~ -30, in_historic_district ~ -25,
      wetlands_pct > 10 ~ -20, TRUE ~ 0
    ),
    development_score = (
      (size_score * 0.20) + (use_score * 0.25) + (location_score * 0.20) +
        (financial_score * 0.15) + (market_score * 0.10) + (zoning_score * 0.10)
    ) + constraint_penalty,
    development_score = pmin(100, pmax(0, development_score)),
    development_tier = case_when(
      development_score >= 75 ~ "Tier 1: High Priority",
      development_score >= 60 ~ "Tier 2: Strong Potential",
      development_score >= 45 ~ "Tier 3: Moderate Potential",
      development_score >= 30 ~ "Tier 4: Limited Potential",
      TRUE ~ "Tier 5: Not Recommended"
    )
  )

# Get top 10
top_10 <- scored_parcels %>%
  arrange(desc(development_score)) %>%
  slice_head(n = 10) %>%
  mutate(rank = row_number())

# UI
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = "VEREP Development Analysis",
    titleWidth = 300
  ),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Executive Summary", tabName = "summary", icon = icon("chart-line")),
      menuItem("Interactive Map", tabName = "map", icon = icon("map")),
      menuItem("Property Rankings", tabName = "rankings", icon = icon("list-ol")),
      menuItem("Property Details", tabName = "details", icon = icon("info-circle")),
      menuItem("Methodology", tabName = "methodology", icon = icon("book")),
      br(),
      downloadButton("download_pptx", "Download PowerPoint", 
                     style = "width: 90%; margin: 10px;"),
      downloadButton("download_excel", "Download Excel Data", 
                     style = "width: 90%; margin: 10px;")
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper { background-color: #f4f6f9; }
        .box { border-top: 3px solid #3c8dbc; }
        .small-box { border-radius: 5px; }
        .nav-tabs-custom { border-radius: 5px; }
      "))
    ),
    
    tabItems(
      # Executive Summary Tab
      tabItem(
        tabName = "summary",
        h2("Executive Summary"),
        
        fluidRow(
          valueBox(
            nrow(scored_parcels), 
            "Properties Analyzed",
            icon = icon("building"),
            color = "blue"
          ),
          valueBox(
            sum(scored_parcels$development_score >= 60),
            "Strong Potential (Tier 1-2)",
            icon = icon("star"),
            color = "green"
          ),
          valueBox(
            round(sum(top_10$area_acres), 1),
            "Top 10 Developable Acres",
            icon = icon("map"),
            color = "yellow"
          ),
          valueBox(
            sum(top_10$financial_need >= 2, na.rm = TRUE),
            "Sites with High Financial Need",
            icon = icon("dollar-sign"),
            color = "red"
          )
        ),
        
        fluidRow(
          box(
            title = "Top 10 Development Priorities",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            DTOutput("top10_table")
          )
        ),
        
        fluidRow(
          box(
            title = "Distribution by Development Tier",
            width = 6,
            status = "primary",
            plotOutput("tier_chart", height = 300)
          ),
          box(
            title = "Key Insights",
            width = 6,
            status = "info",
            HTML("<h4>Strategic Opportunities:</h4>
                 <ul>
                   <li><strong>87 properties</strong> with strong development potential (Tier 1-2)</li>
                   <li><strong>Primary opportunities:</strong> Underutilized parking lots and open space</li>
                   <li><strong>Financial impact:</strong> Development can provide long-term revenue for congregations</li>
                   <li><strong>Mission alignment:</strong> Affordable housing creation supports diocesan values</li>
                 </ul>")
          )
        )
      ),
      
      # Interactive Map Tab
      tabItem(
        tabName = "map",
        h2("Interactive Property Map"),
        
        fluidRow(
          box(
            title = "Filter Properties",
            width = 3,
            status = "primary",
            
            sliderInput("score_range", "Development Score Range:",
                        min = 0, max = 100, value = c(0, 100), step = 5),
            
            checkboxGroupInput("tier_filter", "Development Tiers:",
                               choices = unique(scored_parcels$development_tier),
                               selected = unique(scored_parcels$development_tier)),
            
            sliderInput("size_range", "Property Size (acres):",
                        min = 0, max = max(scored_parcels$area_acres, na.rm = TRUE),
                        value = c(0, max(scored_parcels$area_acres, na.rm = TRUE)),
                        step = 0.5)
          ),
          
          box(
            width = 9,
            leafletOutput("property_map", height = 600)
          )
        )
      ),
      
      # Rankings Tab
      tabItem(
        tabName = "rankings",
        h2("Property Rankings"),
        
        fluidRow(
          box(
            title = "All Properties - Sortable & Searchable",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            DTOutput("full_table")
          )
        )
      ),
      
      # Property Details Tab
      tabItem(
        tabName = "details",
        h2("Property Details"),
        
        fluidRow(
          box(
            title = "Select Property",
            width = 3,
            status = "primary",
            selectInput("selected_property", "Choose Property:",
                        choices = setNames(top_10$objectid, 
                                           paste0("#", top_10$rank, ": ", top_10$site_address)))
          ),
          
          box(
            title = "Property Information",
            width = 9,
            status = "info",
            uiOutput("property_detail")
          )
        ),
        
        fluidRow(
          box(
            title = "Score Breakdown",
            width = 12,
            plotOutput("score_breakdown", height = 300)
          )
        )
      ),
      
      # Methodology Tab
      tabItem(
        tabName = "methodology",
        h2("Methodology & Scoring Framework"),
        
        fluidRow(
          box(
            title = "Development Score Components",
            width = 6,
            status = "primary",
            HTML("<h4>Weighted Dimensions:</h4>
                 <ul>
                   <li><strong>Current Use (25%):</strong> Parking lots and open space score highest</li>
                   <li><strong>Property Size (20%):</strong> Optimal range 0.5-5 acres</li>
                   <li><strong>Location Quality (20%):</strong> Walkability and transit access</li>
                   <li><strong>Financial Need (15%):</strong> Congregation financial health</li>
                   <li><strong>Market Potential (10%):</strong> Income levels and LIHTC eligibility</li>
                   <li><strong>Zoning (10%):</strong> Development-friendly classifications</li>
                 </ul>")
          ),
          
          box(
            title = "Constraint Penalties",
            width = 6,
            status = "warning",
            HTML("<h4>Score Deductions:</h4>
                 <ul>
                   <li><strong>Flood zones:</strong> -40 points</li>
                   <li><strong>Significant wetlands (>25%):</strong> -35 points</li>
                   <li><strong>Easements:</strong> -30 points</li>
                   <li><strong>Historic districts:</strong> -25 points</li>
                   <li><strong>Moderate wetlands (>10%):</strong> -20 points</li>
                 </ul>")
          )
        ),
        
        fluidRow(
          box(
            title = "Development Tiers",
            width = 12,
            status = "info",
            HTML("<table class='table table-striped'>
                   <thead><tr><th>Tier</th><th>Score Range</th><th>Description</th></tr></thead>
                   <tbody>
                     <tr><td><strong>Tier 1</strong></td><td>75-100</td><td>High Priority - Immediate development candidates</td></tr>
                     <tr><td><strong>Tier 2</strong></td><td>60-74</td><td>Strong Potential - Near-term opportunities</td></tr>
                     <tr><td><strong>Tier 3</strong></td><td>45-59</td><td>Moderate Potential - Requires creative solutions</td></tr>
                     <tr><td><strong>Tier 4</strong></td><td>30-44</td><td>Limited Potential - Significant barriers exist</td></tr>
                     <tr><td><strong>Tier 5</strong></td><td>&lt;30</td><td>Not Recommended - Unsuitable for development</td></tr>
                   </tbody>
                 </table>")
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Top 10 Table
  output$top10_table <- renderDT({
    top_10 %>%
      select(rank, site_address, site_city, area_acres, development_score, development_tier) %>%
      datatable(
        options = list(pageLength = 10, dom = 't'),
        rownames = FALSE,
        colnames = c("Rank", "Address", "City", "Acres", "Score", "Tier")
      ) %>%
      formatRound("area_acres", 2) %>%
      formatRound("development_score", 1) %>%
      formatStyle("development_score",
                  background = styleColorBar(c(0, 100), "#3c8dbc"),
                  backgroundSize = "100% 90%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center")
  })
  
  # Tier Distribution Chart
  output$tier_chart <- renderPlot({
    tier_data <- scored_parcels %>%
      count(development_tier) %>%
      mutate(
        pct = n / sum(n) * 100,
        tier_order = case_when(
          str_detect(development_tier, "Tier 1") ~ 1,
          str_detect(development_tier, "Tier 2") ~ 2,
          str_detect(development_tier, "Tier 3") ~ 3,
          str_detect(development_tier, "Tier 4") ~ 4,
          TRUE ~ 5
        ),
        color = case_when(
          tier_order == 1 ~ "#3c8dbc",
          tier_order == 2 ~ "#00a65a",
          tier_order == 3 ~ "#f39c12",
          tier_order == 4 ~ "#dd4b39",
          TRUE ~ "#999999"
        )
      )
    
    ggplot(tier_data, aes(x = reorder(development_tier, -tier_order), y = n, fill = color)) +
      geom_col() +
      geom_text(aes(label = paste0(n, "\n(", round(pct, 1), "%)")), 
                vjust = 1.5, color = "white", size = 5, fontface = "bold") +
      scale_fill_identity() +
      labs(x = "", y = "Number of Properties") +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank()
      )
  })
  
  # Filtered data for map
  filtered_data <- reactive({
    scored_parcels %>%
      filter(
        development_score >= input$score_range[1],
        development_score <= input$score_range[2],
        development_tier %in% input$tier_filter,
        area_acres >= input$size_range[1],
        area_acres <= input$size_range[2]
      )
  })
  
  # Interactive Map
  output$property_map <- renderLeaflet({
    pal <- colorNumeric(palette = "YlOrRd", domain = c(0, 100))
    
    filtered_data() %>%
      leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        lng = ~lon, lat = ~lat,
        radius = ~sqrt(area_acres) * 3,
        fillColor = ~pal(development_score),
        fillOpacity = 0.7,
        color = "white",
        weight = 1,
        popup = ~paste0(
          "<b>", site_address, "</b><br>",
          site_city, ", ", site_county, " County<br>",
          "<hr>",
          "<b>Score:</b> ", round(development_score, 1), "/100<br>",
          "<b>Size:</b> ", round(area_acres, 2), " acres<br>",
          "<b>Tier:</b> ", development_tier
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = c(0, 100),
        title = "Development<br>Score"
      )
  })
  
  # Full Rankings Table
  output$full_table <- renderDT({
    scored_parcels %>%
      select(site_address, site_city, site_county, area_acres, 
             development_score, development_tier, walkability_score,
             use_parking, use_open_space) %>%
      mutate(
        use_parking = ifelse(use_parking, "Yes", "No"),
        use_open_space = ifelse(use_open_space, "Yes", "No")
      ) %>%
      datatable(
        filter = 'top',
        options = list(pageLength = 25, scrollX = TRUE),
        rownames = FALSE,
        colnames = c("Address", "City", "County", "Acres", "Score", 
                     "Tier", "Walkability", "Parking", "Open Space")
      ) %>%
      formatRound(c("area_acres", "development_score", "walkability_score"), 1)
  })
  
  # Property Detail View
  output$property_detail <- renderUI({
    req(input$selected_property)
    
    prop <- top_10 %>% filter(objectid == input$selected_property)
    
    HTML(paste0(
      "<h3>", prop$site_address, "</h3>",
      "<h4>", prop$site_city, ", ", prop$site_county, " County</h4>",
      "<hr>",
      "<div style='font-size: 16px;'>",
      "<p><strong>Development Score:</strong> ", round(prop$development_score, 1), "/100</p>",
      "<p><strong>Tier:</strong> ", prop$development_tier, "</p>",
      "<p><strong>Size:</strong> ", round(prop$area_acres, 2), " acres</p>",
      "<p><strong>Walkability Score:</strong> ", round(prop$walkability_score, 1), "/100</p>",
      if(!is.na(prop$name)) paste0("<p><strong>Congregation:</strong> ", prop$name, "</p>") else "",
      if(!is.na(prop$avg_attendance)) paste0("<p><strong>Avg Attendance:</strong> ", round(prop$avg_attendance), " people</p>") else "",
      "</div>"
    ))
  })
  
  # Score Breakdown Chart
  output$score_breakdown <- renderPlot({
    req(input$selected_property)
    
    prop <- scored_parcels %>% filter(objectid == input$selected_property)
    
    scores <- tibble(
      Component = c("Size", "Use Type", "Location", "Financial Need", "Market", "Zoning"),
      Score = c(prop$size_score, prop$use_score, prop$location_score,
                prop$financial_score, prop$market_score, prop$zoning_score),
      Weight = c(20, 25, 20, 15, 10, 10),
      Weighted = Score * Weight / 100
    )
    
    ggplot(scores, aes(x = reorder(Component, Weighted), y = Weighted)) +
      geom_col(fill = "#3c8dbc") +
      geom_text(aes(label = round(Weighted, 1)), hjust = -0.2, size = 5) +
      coord_flip() +
      ylim(0, max(scores$Weighted) * 1.2) +
      labs(title = "Weighted Score Contribution", x = "", y = "Points") +
      theme_minimal(base_size = 14)
  })
  
  # PowerPoint Export
  output$download_pptx <- downloadHandler(
    filename = function() {
      paste0("VEREP-Analysis-", Sys.Date(), ".pptx")
    },
    content = function(file) {
      # Create PowerPoint
      pres <- read_pptx()
      
      # Title Slide
      pres <- add_slide(pres, layout = "Title Slide", master = "Office Theme")
      pres <- ph_with(pres, value = "VEREP Property Development Analysis", 
                      location = ph_location_type(type = "ctrTitle"))
      pres <- ph_with(pres, value = "Strategic Assessment of Development Opportunities", 
                      location = ph_location_type(type = "subTitle"))
      
      # Executive Summary Slide
      pres <- add_slide(pres, layout = "Title and Content", master = "Office Theme")
      pres <- ph_with(pres, value = "Executive Summary", 
                      location = ph_location_type(type = "title"))
      summary_text <- paste0(
        "• ", nrow(scored_parcels), " properties analyzed across diocese\n",
        "• ", sum(scored_parcels$development_score >= 60), " properties with strong potential (Tier 1-2)\n",
        "• ", round(sum(top_10$area_acres), 1), " acres in top 10 opportunities\n",
        "• ", sum(top_10$financial_need >= 2, na.rm = TRUE), " congregations with high financial need"
      )
      pres <- ph_with(pres, value = summary_text, 
                      location = ph_location_type(type = "body"))
      
      # Top 5 Properties Slide
      pres <- add_slide(pres, layout = "Title and Content", master = "Office Theme")
      pres <- ph_with(pres, value = "Top 5 Development Priorities", 
                      location = ph_location_type(type = "title"))
      
      top5_table <- top_10 %>%
        slice_head(n = 5) %>%
        select(rank, site_address, site_city, area_acres, development_score) %>%
        flextable() %>%
        set_header_labels(rank = "Rank", site_address = "Address", 
                          site_city = "City", area_acres = "Acres", 
                          development_score = "Score") %>%
        autofit()
      
      pres <- ph_with(pres, value = top5_table, 
                      location = ph_location_type(type = "body"))
      
      # Save
      print(pres, target = file)
    }
  )
  
  # Excel Export
  output$download_excel <- downloadHandler(
    filename = function() {
      paste0("VEREP-Data-", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(
        list(
          "Top 10" = top_10 %>% select(-objectid, -lat, -lon),
          "All Properties" = scored_parcels %>% 
            select(site_address, site_city, site_county, area_acres,
                   development_score, development_tier, use_parking, 
                   use_open_space, walkability_score)
        ),
        path = file
      )
    }
  )
}

# Run the app
shinyApp(ui, server)