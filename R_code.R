# Load necessary libraries

library(shiny)
library(shinyjs)
library(leaflet)
library(dplyr)
library(sf)
library(geojsonsf)
library(plotly)
library(shinydashboard)
library(DT)
library(shinythemes)
library(RColorBrewer)
library(scales)
library(stringr)
library(httr)
library(bslib)
library(shinyWidgets)
library(tidyverse)
library(ggiraph)  

# Load external Tableau integration functions
source('tableau-in-shiny-v1.2.R')

##########################################
# Data Loading and Preprocessing         #
##########################################

# Load and preprocess Melbourne precipitation data
rainfall_data <- read.csv('IDCJAC0001_086338_Data1.csv')
rainfall_data <- rainfall_data[!is.na(rainfall_data$Year) & rainfall_data$Year >= 2014 & rainfall_data$Year <= 2023, ]
colnames(rainfall_data) <- c('Product_Code', 'Station_Number', 'Year', 'Month', 'Monthly_Precipitation_Total', 'Quality')

# Load and preprocess Melbourne temperature data
temperature_data <- read.csv('IDCJAC0002_086338_Data1.csv')
temperature_data <- temperature_data[!is.na(temperature_data$Year) & temperature_data$Year >= 2014 & temperature_data$Year <= 2023, ]
colnames(temperature_data) <- c('Product_Code', 'Station_Number', 'Year', 'Month', 'Mean_Max_Temperature', 'Quality')

# Load and parse bicycle routes data
bicycle_data <- read.csv('bicycle-routes-including-informal-on-road-and-off-road-routes.csv')
geo_shapes_bicycle <- lapply(bicycle_data$geo_shape, function(shape) {
  tryCatch({
    geojson_sf(shape)
  }, error = function(e) {
    message("Error parsing shape: ", e)
    return(NULL)
  })
})
geo_shapes_bicycle <- geo_shapes_bicycle[!sapply(geo_shapes_bicycle, is.null)]
if (length(geo_shapes_bicycle) > 0) {
  bicycle_routes_sf <- do.call(rbind, geo_shapes_bicycle)
} else {
  stop("No valid geojson data found in the bicycle routes file.")
}

# Load and parse self-guided walk routes data
self_guided_walks_data <- read.csv('self-guided-walks.csv')
geo_shapes_walks <- lapply(self_guided_walks_data$geo_shape, function(shape) {
  tryCatch({
    geojson_sf(shape)
  }, error = function(e) {
    message("Error parsing shape: ", e)
    return(NULL)
  })
})
geo_shapes_walks <- geo_shapes_walks[!sapply(geo_shapes_walks, is.null)]
if (length(geo_shapes_walks) > 0) {
  walks_routes_sf <- do.call(rbind, geo_shapes_walks)
} else {
  stop("No valid geojson data found in the self-guided walks file.")
}

# Popup information for bicycle and walk routes (if available)
bicycle_popup <- if ("name" %in% colnames(bicycle_routes_sf)) { ~name } else { NULL }
walks_popup <- if ("name" %in% colnames(walks_routes_sf)) { ~name } else { NULL }

# Load and inspect landmarks data
landmarks_data <- read.csv("melbourne_city_landmarks.csv", stringsAsFactors = FALSE)
names(landmarks_data) <- trimws(names(landmarks_data))

# Clean Location data and prepare the dataset
data_cleaned <- landmarks_data %>%
  mutate(
    Location = str_extract(Location, "-?\\d+\\.\\d+\\s*,\\s*-?\\d+\\.\\d+"),
    Latitude = ifelse(!is.na(Location), as.numeric(str_split_fixed(Location, ",\\s*", 2)[, 1]), NA),
    Longitude = ifelse(!is.na(Location), as.numeric(str_split_fixed(Location, ",\\s*", 2)[, 2]), NA),
    Description = str_replace_all(Description, "Ã¯Â¿Â½", ""),
    ILMS_Link = ifelse(!is.na(`ILMS.URI`), 
                       paste0("<br><strong>To view here in the historical photos, please click here: </strong>",
                              "<a href='", `ILMS.URI`, "' target='_blank'>Click here</a>"),
                       "")
  ) %>%
  mutate(
    Latitude = ifelse(grepl("Flinders Street", Description, ignore.case = TRUE), -37.8182711, Latitude),
    Longitude = ifelse(grepl("Flinders Street", Description, ignore.case = TRUE), 144.9670618, Longitude)
  ) %>%
  mutate(
    Full_Description = paste(Description, ILMS_Link)
  ) %>%
  filter(!is.na(Latitude) & !is.na(Longitude))

# Load restaurant data
restaurant_data <- read_csv("cafes-and-restaurants-with-seating-capacity.csv")
area_sf <- st_read("small-areas-for-census-of-land-use-and-employment-clue/small-areas-for-census-of-land-use-and-employment-clue.shp")

# Load Airbnb (Hotels) data
hotels_data <- read.csv("hotels_with_suburb.csv", stringsAsFactors = FALSE)

# Filter options for Airbnb Dashboard
room_types <- c("All", unique(hotels_data$room_type)) 
price_range <- range(hotels_data$price, na.rm = TRUE)
suburbs_airbnb <- c("All", unique(hotels_data$suburb))

###############################
# UI Layout Definition        #
###############################

# Define a function to add map markers for user clicks and assign them to a group
add_map_marker <- function(map_proxy, lat, lng) {
  map_proxy %>%
    addMarkers(lng = lng, lat = lat, popup = paste("Clicked: ", lat, ", ", lng), group = "userMarkers")
}

# Define a function to generate navigation URL
generate_navigation_url <- function(locations) {
  origin <- paste(locations[[1]][["lat"]], locations[[1]][["lng"]], sep = ",")
  destination <- paste(locations[[2]][["lat"]], locations[[2]][["lng"]], sep = ",")
  paste0("https://www.google.com/maps/dir/?api=1&origin=", origin, "&destination=", destination)
}

# Custom CSS for Airbnb Dashboard Tableau iframe
css_airbnb <- "
#tableauViz_airbnb iframe {
    width: 100% !important;
    height: 800px !important;
    overflow: hidden;
}
"

# UI for the Shiny App
ui <- navbarPage(
  header = setUpTableauInShiny(),  # Setup Tableau integration
  theme = shinytheme("yeti"),
  "Discover Melbourne",
  ##################################################
  # Tab 1: Explore Melbourne: Routes & Landmarks   #
  ##################################################
  tabPanel(
    "Explore Melbourne: Routes & Landmarks",
    fluidPage(
      # Adding buttons for navigation and clearing selection
      fluidRow(
        column(3,
               fluidRow(
                 div(
                   style = "display: flex; justify-content: center; gap:140px; width: 100%; margin-bottom: 20px;",
                   actionButton("go_to_navigation_routes", "Start Navigation"), 
                   actionButton("clear_selection_routes", "Reset Map")  
                 )
               )
        ),
        column(9)
      ),
      # Map output remains the same but now interactive with clicks
      leafletOutput("map_routes", width = "100%", height = "80vh") 
    )
  ),
  
  ##################################################
  # Tab 2: Melbourne Precipitation & Temperature   #
  ##################################################
  tabPanel(
    "Melbourne Weather Overview",
    fluidRow(
      column(4, 
             sliderInput("year_range", "Select Year Range:",
                         min = min(rainfall_data$Year),
                         max = max(rainfall_data$Year),
                         value = c(min(rainfall_data$Year), max(rainfall_data$Year)),
                         sep = "",
                         step = 1)
      ),
      column(8)
    ),
    fluidRow(
      column(6, 
             plotlyOutput("combined_plot", height = "600px")
      ),
      column(6, 
             div(style = "margin-top: 50px;",
                 tableauPublicViz(
                   id = "tableauviz",
                   url = 'https://public.tableau.com/views/solarexplosurev2/AverageMonthlySolarExposure?:language=zh-CN&publish=yes&:display_count=n&:origin=viz_share_link',
                   height = '600px',
                   options = list(
                     hideTabs = TRUE,
                     hideToolbar = TRUE
                   )
                 )
             )
      )
    )
  ),
  
  ##########################################
  # Tab 3: "Where to Eat in Melbourne      #
  ##########################################
  tabPanel(
    "Where to Eat in Melbourne",
    useShinyjs(),
    sidebarLayout(
      sidebarPanel(
        h4("Filters"),
        pickerInput(
          inputId = "seating_type",
          label = "Select Seating Type",
          choices = unique(restaurant_data$`Seating type`),
          selected = unique(restaurant_data$`Seating type`),
          multiple = TRUE,
          options = list(`actions-box` = TRUE, `live-search` = TRUE)
        ),
        pickerInput(
          inputId = "industry",
          label = "Select Industry",
          choices = unique(restaurant_data$`Industry (ANZSIC4) description`),
          selected = unique(restaurant_data$`Industry (ANZSIC4) description`),
          multiple = TRUE,
          options = list(`actions-box` = TRUE, `live-search` = TRUE)
        ),
        pickerInput(
          inputId = "clue_area",
          label = "Select Suburb",
          choices = unique(restaurant_data$`Suburb`),
          selected = unique(restaurant_data$`Suburb`),
          multiple = TRUE,
          options = list(`actions-box` = TRUE, `live-search` = TRUE)
        ),
        sliderInput(
          inputId = "number_of_seats",
          label = "Number of Seats",
          min = min(restaurant_data$`Number of seats`, na.rm = TRUE),
          max = 200,
          value = c(min(restaurant_data$`Number of seats`, na.rm = TRUE), 200),
          step = 1
        ),
        width = 3,
        style = "background-color: white;" 
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Locations",
            h4("Map for Restaurants"),
            fluidRow(
              column(12, leafletOutput("restaurant_map", width = "100%", height = "600px"))
            )
          ),
          tabPanel(
            "Restaurant Characteristics & Preview",
            h4("Restaurant Characteristics"),
            fluidRow(
              column(6, plotOutput("restaurant_type_plot", height = "400px")),
              column(6, plotOutput("restaurant_area_plot", height = "400px"))
            ),
            fluidRow(
              column(12,
                     h4("Restaurant Preview"),
                     dataTableOutput("restaurant_preview", width = "100%")
              )
            )
          )
        ),
        width = 9
      )
    )
  )
  ,
  
  #######################################
  # Tab 4: Airbnb Dashboard & Analysis  #
  #######################################
  tabPanel(
    "Where to Stay in Melbourne",
    useShinyjs(),
    tags$head(
      tags$style(HTML(css_airbnb))
    ),
    
    # Main panel with tabsetPanel for Map and Analysis
    mainPanel(
      tabsetPanel(
        # Tab 1: Locations - This will include the sidebar
        tabPanel(
          "Locations",
          sidebarLayout(
            # Sidebar panel for filters in "Locations"
            sidebarPanel(
              h4("Filters"),
              sliderInput("priceRange",
                          "Price Range:",
                          min = price_range[1],
                          max = price_range[2],
                          value = c(300, 600),
                          step = 10),
              selectInput("roomType",
                          "Room Type:",
                          choices = room_types,
                          selected = "All"),
              selectInput("suburb_airbnb",
                          "Suburb:",
                          choices = suburbs_airbnb,
                          selected = "All"),
              width = 3  
            ),
            
            # Main content for the "Locations" tab
            mainPanel(
              h4("Map for Airbnb"),
              fluidRow(
                column(12, leafletOutput("map_airbnb", width = "100%", height = "800px"))  
              ),
              textOutput("selected_hotel"),
              textOutput("no_data_message")
            )
          )
        ),
        
        # Tab 2: Airbnb Analysis
        tabPanel(
          "Airbnb Analysis",
          fluidRow(
            column(6,
                   tableauPublicViz(
                     id = 'tableauViz_airbnb_left',
                     url = 'https://public.tableau.com/views/AvgPriceperSuburb/AvgPriceperSuburb?:language=zh-CN&publish=yes&:sid=&:redirect=auth&:display_count=n&:origin=viz_share_link',
                     height = "800px",  
                     width = "100%",
                     options = list(
                       hideTabs = TRUE,
                       hideToolbar = TRUE
                     )
                   )
            ),
            column(6,
                   tableauPublicViz(
                     id = 'tableauViz_airbnb_right',
                     url = 'https://public.tableau.com/views/PieChartforRoomType/PieChartforRoomType?:language=en-GB&publish=yes&:sid=&:redirect=auth&:display_count=n&:origin=viz_share_link',
                     height = "800px",
                     width = "100%",
                     options = list(
                       hideTabs = TRUE,
                       hideToolbar = TRUE
                     )
                   )
            )
          )
        )
      ),
      width = 11
    )
  )
  
)

###############################
# Server Logic Definition     #
###############################
server <- function(input, output, session) {
  
  ##################################################
  # Tab 1: Melbourne Precipitation & Temperature   #
  ##################################################
  
  # Data Processing for Graphs
  filtered_data_precip_temp <- reactive({
    req(input$year_range)
    
    precip_filtered <- rainfall_data %>%
      filter(Year >= input$year_range[1] & Year <= input$year_range[2])
    
    temp_filtered <- temperature_data %>%
      filter(Year >= input$year_range[1] & Year <= input$year_range[2])
    
    list(
      precip_data = precip_filtered,
      temp_data = temp_filtered
    )
  })
  
  output$combined_plot <- renderPlotly({
    precip_data <- filtered_data_precip_temp()$precip_data %>%
      group_by(Month) %>%
      summarise(Average_Precipitation = mean(Monthly_Precipitation_Total, na.rm = TRUE))
    
    temp_data <- filtered_data_precip_temp()$temp_data %>%
      group_by(Month) %>%
      summarise(Average_Temperature = mean(Mean_Max_Temperature, na.rm = TRUE))
    
    plot_ly() %>%
      add_bars(x = ~precip_data$Month, y = ~precip_data$Average_Precipitation, 
               name = 'Precipitation (mm)', marker = list(color = '#92A8D1'), yaxis = 'y') %>%
      add_lines(x = ~temp_data$Month, y = ~temp_data$Average_Temperature, 
                name = 'Temperature (°C)', line = list(color = '#FF6F61'), yaxis = 'y2') %>%
      layout(
        title = list(
          text = paste('Melbourne Precipitation & Temperature (', input$year_range[1], '-', input$year_range[2], ')'),
          x = 0.5,
          xanchor = "center"
        ),
        xaxis = list(
          title = 'Month',
          tickvals = 1:12,
          ticktext = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
        ),
        yaxis = list(
          title = 'Average Precipitation (mm)',
          rangemode = "tozero"
        ),
        yaxis2 = list(
          title = 'Average Temperature (°C)',
          overlaying = 'y',
          side = 'right',
          rangemode = "tozero"
        ),
        margin = list(t = 100, r = 50),
        legend = list(
          x = 0,
          xanchor = "left",
          y = 1.1
        )
      )
  })
  
  ############################################
  # Tab 2: Bicycle & Self-Guided Walk Routes #
  ############################################
  
  # Getting the weather data
  url_weather <- "https://api.openweathermap.org/data/2.5/weather"
  params_weather <- list(
    lat = "-37.8136",
    lon = "144.9631",
    appid = "22fadead3fc7b8b1d3adb02b50c0b1c1",
    units = "metric"
  )
  
  response_weather <- GET(url_weather, query = params_weather)
  
  
  if (status_code(response_weather) == 200) {
    weather_data <- content(response_weather, as = "parsed", type = "application/json")
    weather_info <- paste(
      "<div style='padding: 10px; border-radius: 8px; background: #f0f0f0; box-shadow: 2px 2px 5px rgba(0,0,0,0.1);'>",
      "<h4 style='margin: 0; font-size: 16px; color: #333;'>Current Weather in Melbourne</h4>",
      "<p style='margin: 5px 0; font-size: 14px; color: #555;'>",
      "Weather: <strong>", weather_data$weather[[1]]$description, "</strong><br>",
      "Temperature: <strong>", weather_data$main$temp, "°C</strong><br>",
      "Humidity: <strong>", weather_data$main$humidity, "%</strong><br>",
      "Wind Speed: <strong>", weather_data$wind$speed, "m/s</strong>",
      "</p>",
      "</div>"
    )
  } else {
    weather_info <- "<div style='padding: 10px; border-radius: 8px; background: #f0f0f0; color: #555; box-shadow: 2px 2px 5px rgba(0,0,0,0.1);'>Unable to fetch weather data.</div>"
  }
  
  clicked_locations_routes <- reactiveVal(list())
  
  # Map rendering with bicycle and self-guided walk routes
  output$map_routes <- renderLeaflet({
    m <- leaflet() %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      setView(lng = 144.96551628872666, lat = -37.810269282621164, zoom = 14)
    
    # Add Bicycle routes layer
    m <- m %>%
      addPolylines(data = bicycle_routes_sf,
                   color = "#0000CD",
                   weight = 2,
                   group = "Bicycle Routes",
                   popup = bicycle_popup)
    
    # Add Self-Guided Walk routes layer
    m <- m %>%
      addPolylines(data = walks_routes_sf,
                   color = "red",
                   weight = 2,
                   group = "Self-Guided Walks",
                   popup = walks_popup)
    
    # Add Landmarks layer
    m <- m %>%
      addMarkers(
        data = data_cleaned,
        lng = ~Longitude,
        lat = ~Latitude,
        popup = ~paste("<strong>Title:</strong>", Title, "<br><strong>Description:</strong>", Full_Description),
        label = ~Title,
        group = "Landmarks"
      )
    
    # Add weather information control
    m <- m %>%
      addControl(html = weather_info, position = "topleft", className = "weather-info")
    
    # Add layer control
    m <- m %>%
      addLayersControl(
        position = "topright",
        overlayGroups = c("Bicycle Routes", "Self-Guided Walks", "Landmarks"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      hideGroup("Bicycle Routes") %>%
      hideGroup("Self-Guided Walks") %>%
      hideGroup("Landmarks")
    
    m
  })
  
  # Monitor map clicks and update the clicked locations
  observeEvent(input$map_routes_click, {
    event <- input$map_routes_click
    if (length(clicked_locations_routes()) < 2) {
      new_locations <- c(clicked_locations_routes(), list(c(lat = event$lat, lng = event$lng)))
      clicked_locations_routes(new_locations)
      
      leafletProxy("map_routes") %>% add_map_marker(event$lat, event$lng)
    }
  })
  
  # Monitor navigation button clicks to open Google Maps
  observeEvent(input$go_to_navigation_routes, {
    locations <- clicked_locations_routes()
    if (length(locations) == 2) {
      browseURL(generate_navigation_url(locations))
    }
  })
  
  # Clear only the user-selected markers
  observeEvent(input$clear_selection_routes, {
    clicked_locations_routes(list())
    leafletProxy("map_routes") %>%
      clearGroup("userMarkers")
  })
  
  ###############################
  # Tab 3: Restaurant Analysis  #
  ###############################
  
  # Reactive data filtering for restaurant data
  filtered_restaurant_data <- reactive({
    restaurant_data %>%
      filter(
        `Seating type` %in% input$seating_type,
        `Industry (ANZSIC4) description` %in% input$industry,
        `Suburb` %in% input$clue_area,
        `Number of seats` >= input$number_of_seats[1],
        `Number of seats` <= input$number_of_seats[2]
      )
  })
  
  # Reactive expression to aggregate restaurant data by suburb
  aggregated_restaurant_data <- reactive({
    filtered_restaurant_data() %>%
      group_by(`Suburb`) %>%
      summarise(
        restaurant_count = n(),
        Latitude = mean(Latitude, na.rm = TRUE),
        Longitude = mean(Longitude, na.rm = TRUE)
      ) %>%
      ungroup()
  })
  
  # Render Leaflet map for restaurants
  output$restaurant_map <- renderLeaflet({
    leaflet(filtered_restaurant_data()) %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        popup = ~paste(
          "<strong>CLUE Area:</strong>", `Suburb`, "<br>",
          "<strong>Trading Name:</strong>", `Trading name`, "<br>",
          "<strong>Business Address:</strong>", `Business address`, "<br>",
          "<strong>Seating Type:</strong>", `Seating type`, "<br>",
          "<strong>Number of Seats:</strong>", `Number of seats`
        ),
        color = '#FFD700',
        fillColor = '#FFD700',
        fillOpacity = 0.8,
        clusterOptions = markerClusterOptions()
      ) %>%
      setView(lat = -37.81329, lng = 144.9629, zoom = 12)
  })
  
  # Render bar chart
  output$restaurant_type_plot <- renderPlot({
    top_industry <- names(which(sort(table(filtered_restaurant_data()$`Industry (ANZSIC4) description`), decreasing = TRUE) > 1000))
    
    filtered_restaurant_data() %>%
      mutate(Industry = ifelse(`Industry (ANZSIC4) description` %in% top_industry, 
                               `Industry (ANZSIC4) description`, "Other")) %>%
      mutate(Industry = factor(Industry, levels = rev(c(top_industry, "Other")))) %>%
      count(`Seating type`, Industry) %>%
      ggplot(aes(x = `Seating type`, y = n, fill = Industry)) +
      geom_col(position = "dodge", color = "black", alpha = 0.8) +
      labs(
        x = "", y = "Count", 
        title = "Restaurant Count by Seating Type" 
      ) +
      scale_fill_manual(
        values = c("#08306b", "#2171b5", "#4292c6", "#6baed6", "#c6dbef")
      ) +
      coord_flip() +
      theme_classic() +
      theme(
        plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16)
      )
  })
  
  # Render Suburb map
  output$restaurant_area_plot <- renderPlot({
    area_sf %>%
      left_join(filtered_restaurant_data() %>% count(`Suburb`), by = c("featurenam" = "Suburb")) %>%
      ggplot() +
      geom_sf(aes(fill = n)) +
      scale_fill_distiller(palette = "Blues", direction = 1, na.value = "grey90") +
      labs(
        title = "Restaurant Distribution by Suburb" 
      ) +
      theme_void() +
      theme(
        plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16)
      )
  })
  
  # Render data preview table
  output$restaurant_preview <- renderDataTable({
    datatable(
      filtered_restaurant_data() %>%
        select(-`Census year`, -`Block ID`, -`Property ID`, -`Base property ID`, -location, -`Industry (ANZSIC4) code`, -Longitude, -Latitude),
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  ######################################
  # Tab 4: Airbnb Dashboard & Analysis #
  ######################################
  
  # Reactive expression for filtered Airbnb data
  filtered_airbnb <- reactive({
    data <- hotels_data %>%
      filter(price >= input$priceRange[1], price <= input$priceRange[2]) %>%
      filter(!is.na(Longitude) & !is.na(Latitude))
    
    if (input$roomType != "All") {
      data <- data %>%
        filter(room_type == input$roomType)
    }
    
    if (input$suburb_airbnb != "All") {
      data <- data %>%
        filter(suburb == input$suburb_airbnb)
    }
    
    if (nrow(data) == 0) {
      print("No data after filtering, returning NULL")
      return(NULL)
    }
    
    return(data)
  })
  
  # Render a message if no data is found
  output$no_data_message <- renderText({
    if (is.null(filtered_airbnb())) {
      return("No data available for the selected filters.")
    } else {
      return("")  # No message when data is available
    }
  })
  
  # Render Leaflet map for Airbnb
  output$map_airbnb <- renderLeaflet({
    data <- filtered_airbnb()
    
    if (is.null(data) || nrow(data) == 0) {
      return(NULL)  # Safely exit if there is no data to display
    }
    
    pal <- colorNumeric(palette = "YlOrRd", domain = data$price)
    
    # Render leaflet map with clustering
    leaflet(data) %>%
      addProviderTiles(providers$OpenStreetMap) %>%  
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        color = "black",  
        fillColor = ~pal(price), 
        label = ~paste0("Price: $", price),  
        popup = ~paste0(
          "<b>Host Name:</b> ", host_name, "<br>",
          "<b>Price:</b> $", price, "<br>",
          "<b>Rating:</b> ", rating
        ), 
        radius = 8,  
        fillOpacity = 0.9,
        stroke = TRUE,
        weight = 2,  
        clusterOptions = markerClusterOptions()  
      ) %>%
      addLegend(
        "bottomright", pal = pal, values = ~price,
        title = "Price Range",
        labFormat = labelFormat(prefix = "$"),
        opacity = 1
      )
  })
  
  
  # Display selected hotel information
  output$selected_hotel <- renderText({
    paste("You have selected hotels in suburb:", input$suburb_airbnb, "of type:", input$roomType)
  })
  
}

###############################
# Run the Shiny Application   #
###############################

# Shiny application
shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
