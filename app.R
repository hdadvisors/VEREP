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


# Load PRE-PROCESSED data (same as Quarto)
property_profile <- read_csv("data/output/verep_full_analysis.csv")

# Use the same processing as Quarto
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
    assessed_value = lan_val,
    use_church = !is.na(church) & church == 1,
    use_cemetery = !is.na(cemetery) & cemetery == 1,
    use_school = !is.na(school) & school == 1,
    use_parking = !is.na(parking) & parking == 1,
    use_open_space = !is.na(open_space) & open_space == 1,
    use_residence = !is.na(residence) & residence == 1,
    walkability_score = replace_na(walk_idx, 0),
    site_address = sadd,
    site_city = scity,
    site_county = scounty,
    # Use existing scores from preprocessed file or set defaults
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
    location_score = pmin(100, pmax(0, walkability_score)),
    financial_score = case_when(
      !is.na(pct_change_pledge) & pct_change_pledge < -20 ~ 100,
      !is.na(pct_change_pledge) & pct_change_pledge < 0 ~ 70,
      TRUE ~ 30
    ),
    market_score = 50,  # Default
    zoning_score = 50,  # Default
    # Congregation data
    avg_attendance = attendance_2023,
    avg_members = 0,
    avg_pledge = plate_pledge_2023,
    financial_need = case_when(
      !is.na(pct_change_pledge) & pct_change_pledge < -20 ~ 3,
      !is.na(pct_change_pledge) & pct_change_pledge < 0 ~ 2,
      TRUE ~ 1
    ),
    name = congregation_name
  ) %>%
  filter(!is.na(area_acres), area_acres > 0.1, !is.na(lat) & !is.na(lon))

# ADU design library
adu_designs <- tibble(
  name = c("Modal 01", "Dwight", "Compact Studio", "Large 2BR"),
  sqft = c(432, 594, 350, 850),
  width_ft = c(20, 25, 15, 30),
  length_ft = c(22, 24, 23, 28),
  bedrooms = c(1, 1, 0, 2),
  color = c("#445ca9", "#8baeaa", "#e85d75", "#f39c12")
)

# Function to convert feet to approximate degrees
ft_to_degrees <- function(feet, lat) {
  ft_to_deg_lat <- 1 / 364000
  ft_to_deg_lng <- 1 / (288200 * cos(lat * pi / 180))
  list(lat = feet * ft_to_deg_lat, lng = feet * ft_to_deg_lng)
}

# Function to create ADU polygon at a point
create_adu_polygon <- function(lng, lat, width_ft, length_ft) {
  deg <- ft_to_degrees(1, lat)
  
  half_width <- (width_ft * deg$lng) / 2
  half_length <- (length_ft * deg$lat) / 2
  
  coords <- matrix(c(
    lng - half_width, lat - half_length,
    lng + half_width, lat - half_length,
    lng + half_width, lat + half_length,
    lng - half_width, lat + half_length,
    lng - half_width, lat - half_length
  ), ncol = 2, byrow = TRUE)
  
  st_polygon(list(coords)) %>%
    st_sfc(crs = 4326)
}

# Get top 10 using SAME LOGIC as Quarto
top_10 <- scored_parcels %>%
  filter(development_potential %in% c("High Potential", "Moderate Potential")) %>%
  arrange(desc(assessed_value)) %>%  # Sort by land value, not score
  slice_head(n = 10) %>%
  mutate(
    rank = row_number(),
    objectid = uid  # Use existing unique ID from data
  )

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
      menuItem("ADU Placement Tool", tabName = "adu", icon = icon("home")),
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
      
      # ADU Placement Tab
      tabItem(
        tabName = "adu",
        h2("Interactive ADU Placement Tool"),
        
        fluidRow(
          box(
            title = "Select Property",
            width = 12,
            status = "primary",
            selectInput("adu_property", "Choose a property to explore:",
                        choices = setNames(top_10$objectid, 
                                           paste0("#", top_10$rank, ": ", 
                                                  top_10$site_address, " - ", 
                                                  top_10$site_city)))
          )
        ),
        
        fluidRow(
          box(
            title = "ADU Design Controls",
            width = 3,
            status = "primary",
            
            selectInput("adu_design", "Select ADU Design:",
                        choices = adu_designs$name,
                        selected = adu_designs$name[1]),
            
            uiOutput("adu_design_info"),
            
            hr(),
            
            actionButton("clear_adus", "Clear All ADUs", 
                         class = "btn-warning",
                         style = "width: 100%; margin-bottom: 10px;"),
            
            actionButton("reset_view", "Reset View",
                         class = "btn-secondary",
                         style = "width: 100%;"),
            
            hr(),
            
            div(
              style = "font-size: 0.9em; color: #666;",
              h5("How to use:"),
              tags$ol(
                tags$li("Select a property from the dropdown above"),
                tags$li("Choose an ADU design"),
                tags$li("Click on the map to place the ADU"),
                tags$li("Place multiple ADUs to explore layouts"),
                tags$li("Toggle between Satellite and Street views")
              ),
              br(),
              div(
                style = "padding: 10px; background: #d1ecf1; border-radius: 5px;",
                icon("info-circle"), " Use satellite view to see existing structures and identify open areas."
              )
            )
          ),
          
          box(
            title = "Property Map",
            width = 9,
            status = "info",
            leafletOutput("adu_map", height = 700)
          )
        ),
        
        fluidRow(
          box(
            title = "Property Information",
            width = 12,
            status = "info",
            uiOutput("adu_property_info")
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
  
  # ADU Placement Functionality
  
  # Reactive values for ADU placement
  adu_state <- reactiveValues(
    placed_adus = list(),
    counter = 0
  )
  
  # Get selected property
  selected_adu_property <- reactive({
    req(input$adu_property)
    top_10 %>% filter(objectid == input$adu_property)
  })
  
  # Display ADU design info
  output$adu_design_info <- renderUI({
    design <- adu_designs %>% filter(name == input$adu_design)
    
    div(
      style = sprintf(
        "background: %s; color: white; padding: 15px; border-radius: 5px; margin-top: 10px;",
        design$color
      ),
      h4(design$name, style = "margin: 0 0 10px 0; color: white;"),
      tags$div(
        style = "display: grid; grid-template-columns: 1fr 1fr; gap: 10px;",
        div(tags$strong("Size:"), tags$br(), paste(design$sqft, "sqft")),
        div(tags$strong("Bedrooms:"), tags$br(), design$bedrooms),
        div(
          style = "grid-column: 1 / -1;",
          tags$strong("Dimensions:"), tags$br(),
          paste(design$width_ft, "ft ×", design$length_ft, "ft")
        )
      )
    )
  })
  
  # Display property information
  output$adu_property_info <- renderUI({
    prop <- selected_adu_property()
    
    HTML(paste0(
      "<div style='display: grid; grid-template-columns: repeat(4, 1fr); gap: 20px;'>",
      "<div><strong>Address:</strong><br>", prop$site_address, "<br>", prop$site_city, "</div>",
      "<div><strong>Size:</strong><br>", round(prop$area_acres, 2), " acres</div>",
      "<div><strong>Development Score:</strong><br>", round(prop$development_score, 1), "/100</div>",
      "<div><strong>Walkability:</strong><br>", round(prop$walkability_score, 1), "/100</div>",
      "</div>"
    ))
  })
  
  # Initialize map
  output$adu_map <- renderLeaflet({
    prop <- selected_adu_property()
    
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Street") %>%
      addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
      addLayersControl(
        baseGroups = c("Satellite", "Street", "OpenStreetMap"),
        position = "topright"
      ) %>%
      setView(lng = prop$lon, lat = prop$lat, zoom = 19) %>%
      addMarkers(
        lng = prop$lon,
        lat = prop$lat,
        label = prop$site_address,
        popup = paste0(
          "<strong>", prop$site_address, "</strong><br>",
          "Land Value: $", format(prop$assessed_value, big.mark = ","), "<br>",
          "Acreage: ", round(prop$area_acres, 2)
        ),
        group = "Property Marker"
      ) %>%
      addScaleBar(position = "bottomleft") %>%
      addMeasure(
        position = "topleft",
        primaryLengthUnit = "feet",
        primaryAreaUnit = "sqfeet",
        activeColor = "#3c8dbc",
        completedColor = "#00a65a"
      )
  })
  
  # Update map when property changes
  observeEvent(input$adu_property, {
    prop <- selected_adu_property()
    
    leafletProxy("adu_map") %>%
      clearMarkers() %>%
      clearGroup("ADUs") %>%
      setView(lng = prop$lon, lat = prop$lat, zoom = 19) %>%
      addMarkers(
        lng = prop$lon,
        lat = prop$lat,
        label = prop$site_address,
        popup = paste0(
          "<strong>", prop$site_address, "</strong><br>",
          "Land Value: $", format(prop$assessed_value, big.mark = ","), "<br>",
          "Acreage: ", round(prop$area_acres, 2)
        ),
        group = "Property Marker"
      )
    
    # Reset ADU state
    adu_state$placed_adus <- list()
    adu_state$counter <- 0
  })
  
  # Place ADU on map click
  observeEvent(input$adu_map_click, {
    click <- input$adu_map_click
    design <- adu_designs %>% filter(name == input$adu_design)
    
    # Create ADU geometry
    adu_geom <- create_adu_polygon(
      click$lng, click$lat,
      design$width_ft, design$length_ft
    )
    
    adu_state$counter <- adu_state$counter + 1
    adu_id <- paste0("adu_", adu_state$counter)
    
    # Add to map
    leafletProxy("adu_map") %>%
      addPolygons(
        data = adu_geom,
        layerId = adu_id,
        fillColor = design$color,
        fillOpacity = 0.7,
        weight = 2,
        color = "white",
        label = paste(design$name, "-", design$sqft, "sqft"),
        popup = paste0(
          "<strong>", design$name, "</strong><br>",
          "Size: ", design$sqft, " sqft<br>",
          "Dimensions: ", design$width_ft, "' × ", design$length_ft, "'<br>",
          "Bedrooms: ", design$bedrooms
        ),
        group = "ADUs"
      )
    
    # Store ADU data
    adu_state$placed_adus[[adu_id]] <- list(
      geom = adu_geom,
      design = design$name,
      sqft = design$sqft,
      lng = click$lng,
      lat = click$lat
    )
  })
  
  # Clear all ADUs
  observeEvent(input$clear_adus, {
    leafletProxy("adu_map") %>%
      clearGroup("ADUs")
    adu_state$placed_adus <- list()
    adu_state$counter <- 0
  })
  
  # Reset view
  observeEvent(input$reset_view, {
    prop <- selected_adu_property()
    leafletProxy("adu_map") %>%
      setView(lng = prop$lon, lat = prop$lat, zoom = 19)
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