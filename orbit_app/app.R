# Visualise planetary orbits from a scaled model of the Sun
# John Godlee (johngodlee@gmail.com)
# 2020-04-04

# Packages ----
library(shiny)
library(leaflet)
library(rgdal)
library(dplyr)

# UI ----
ui <- bootstrapPage(
  # Define some CSS
  tags$style(
    type = "text/css",
    "html, body {width:100%;height:100%}",
    ".leaflet .legend {
      line-height: 25px;
      font-size: 25px;
      border-radius: 15px;
      padding: 20px;
    }",
    ".leaflet .legend i{
      width: 30px;
      height: 30px;
    }",
    "#panel {background-color: white;
      padding: 20px;
      border-radius: 15px;
      opacity: 0.9;}",
    "#button, #collapse {background-color: #1E5C3F;
      color: #FFFFFF;
      font-size: 2rem;
      margin: 5px 0px;}",
    "#scale {font-size: 2rem;}"
  ),
  
  # UI elements
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(id = "panel", top = 10, right = 10,
    HTML("<button id='collapse' 
      class='btn btn-default action-button shiny-bound-input' 
      data-toggle='collapse' 
      data-target='#content'>Collapse sidebar</button>"),
    tags$div(id = 'content',  class="collapse in",
    numericInput("diam", "Diameter of model Sun (cm)", 
      min = 1, max = 1000, value = 30, step = 1),
    tableOutput("table"),
    textOutput("scale"),
    br(),
    actionButton("button", "Instructions"),
    checkboxInput("pluto", "Show pluto?", value = FALSE))
  )
)

server <- function(input, output, session) {
  # Diameter of Sun
  DSun <- 1.39 * 10 ^ 6
  
  # Read in orbits spatial data
  coords_spdf <- readOGR("orbits_shp", "orbits")
  
  # Planet distances and diameters
  planets <- read.csv("planet_dist_diam.csv")
  
  # Function to render scientific notation
  changeSciNot <- function(n) {
    output <- format(n, scientific = TRUE, digits = 2) #Transforms the number into scientific notation even if small
    output <- sub("e", "x10^", output) #Replace e with 10^
    output <- sub("\\+0?", "", output) #Remove + symbol and leading zeros on expoent, if > 1
    output <- sub("-0?", "-", output) #Leaves - symbol but removes leading zeros on expoent, if < 1
    output <- gsub("\\^", "<sup>", output)
    output <- paste0(output, "</sup>")
    output
  }
  
  # Function to convert latlong to UTM
  latlong2utm <- function(x, y) {
    paste((floor((x + 180) / 6) %% 60) + 1,
      ifelse(y < 0, "S", "N"),
      sep = "")
  }
  
  # Calculate UTM zone of click
  utmzone <- reactive({
    latlong2utm(as.numeric(input$map_click[2]), as.numeric(input$map_click[1]))
  })
  
  # Create valid proj4string of click UTM
  utmproj4string <- reactive({
    paste0(
      "+proj=utm +zone=",
      gsub("[A-z]", "", as.character(utmzone())),
      ifelse(gsub("[0-9]", "", as.character(utmzone())) == "S", " +south", ""),
      " +ellps=WGS84"
    )
  })
  
  # SpatialPoints object of click
  centre_latlong <- reactive({
    SpatialPoints(
      coords = data.frame(lon = c(as.numeric(input$map_click[2])), lat = c(as.numeric(input$map_click[1]))),
      proj4string = CRS("+proj=longlat +ellps=WGS84")
    )
  })
  
  # Transform centre to UTM
  centre_utm <- reactive({
    spTransform(centre_latlong(), CRS(as.character(utmproj4string())))
  })
  
  # Convert Orbits to metres, then scale (to Sun model) and transform (to chosen centre) coordinates of orbits
  coords_utm_spdf <- reactive({
    req(input$map_click)
    poly_list <- lapply(1:length(coords_spdf), function(x){
      coords <- coords_spdf@polygons[[x]]@Polygons[[1]]@coords
      coords <- coords * 1000  # Convert to metres
      coords <- coords / (DSun / (input$diam / 10 ^ 5))  # Adjust scale to Sun model
      coords[, 1] <- coords[, 1] + centre_utm()@coords[1]  # Change centre
      coords[, 2] <- coords[, 2] + centre_utm()@coords[2]
      coords <- Polygon(coords)
      return(coords)
    })
    
    coords_poly <- lapply(seq_along(poly_list), function(x){
      Polygons(list(poly_list[[x]]),
        ID = x)
    })
    
    coords_sp <- SpatialPolygons(coords_poly, proj4string = CRS(as.character(utmproj4string())))
    
    coords_utm_spdf <- SpatialPolygonsDataFrame(coords_sp,
      data.frame(id = coords_spdf@data$id),)
    
    coords_utm_spdf@data$id <- factor(coords_utm_spdf@data$id, 
      levels = tolower(planets$planet_names),
      labels = planets$planet_names)
    
    if(input$pluto == FALSE)
      coords_utm_spdf <- coords_utm_spdf[1:8,]
      
    
    return(coords_utm_spdf)
  })
  
  # Create a dataframe with distances of planets
  stats_df <- reactive({
    stats_df <- data.frame(
      planet_names = planets$planet_names,
      planet_dist = changeSciNot(planets$planet_dist),
      scale_dist = planets$planet_dist * ((input$diam / 10^5) / DSun) * 1000,
      scale_diam = planets$planet_diam * ((input$diam / 10^5) / DSun) * 100000
    )
    if(input$pluto == FALSE)
      stats_df <- stats_df %>% filter(planet_names != "Pluto")
    return(stats_df)
  })
  
  # Convert orbits back to WGS84-LatLong
  coords_wgs84_spdf <- reactive({
    spTransform(coords_utm_spdf(), CRS = ("+proj=longlat +ellps=WGS84"))
  })
  
  # Create Leaflet map static elements
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldTopoMap,
        options = providerTileOptions(noWrap = TRUE)) %>%
      setView(lng = -3.2, lat = 55.9, 
        zoom = 5)
  })
  
  # Define a colour palette
  pal <- reactive({
    colorFactor(
      palette = c("#1B9E77", "#D95F02", "#7570B3", 
        "#E7298A", "#66A61E", "#E6AB02",
        "#A6761D", "#666666", "#000000"),
      domain = coords_wgs84_spdf()$id
    )
  })
  
  # Create Leaflet map dynamic elements
  observe({
    req(input$map_click)
    leafletProxy("map") %>%
      clearShapes() %>%
      clearMarkers() %>%
      clearControls() %>%
      addCircleMarkers(
        lng = as.numeric(input$map_click[2]), lat = as.numeric(input$map_click[1]),
        radius = 2, 
        color = "black", stroke = FALSE, fillOpacity = 1) %>%
      addPolygons(
        data = coords_wgs84_spdf(),
        color = ~pal()(id),
        weight = 2,
        smoothFactor = 0.5,
        opacity = 1.0,
        fillOpacity = 0
      ) %>%
      addLegend(data = coords_wgs84_spdf(), "bottomright", 
        pal = pal(), values = ~id,
        title = "Planet orbit",
        opacity = 1
      ) %>% 
      setView(lng = input$map_click[2], lat = input$map_click[1], 
        zoom = input$map_zoom)
  })
  
  # Create table of orbit distances
  output$table <- renderTable(
    stats_df() %>%
      rename("Planet" = planet_names,
        "Real distance (km)" = planet_dist,
        "Scaled distance (m)" = scale_dist,
        "Scaled diameter (mm)" = scale_diam),
    sanitize.text.function = function(x) x)
  
  # Function to format very large numbers as millions, billions etc.
  mill_format <- function(x) { 
    div <- findInterval(as.numeric(x), 
      c(0, 1e3, 1e6, 1e9, 1e12, 1e15) )  # modify this if negative numbers are possible
    paste0(round( as.numeric(gsub("\\,","",x))/10^(3*(div-1)), 2), 
      c("","K","M","B","T", "Q")[div] )}
  
  # Create scale factor text
  output$scale <- renderText(
    paste0("Scale factor: ",
      mill_format(DSun / ((input$diam / 10^5) / DSun)),
      ":1")
  )
  
  # Define instructions popup
  observeEvent(input$button, {
    showModal(modalDialog(
      title = "Instructions",
      HTML("
      <ol type='1'>
        <li>Find a suitable spherical model of the Sun, like a golf ball, or a space-hopper.</li>
        <li>Measure its diameter in cm.</li>
        <li>Enter the diameter of your model Sun into this app.</li>
        <li>Click a location on the map to see the relative size of the planetary orbits for your model Sun.</li>
      </ol>
      <br>
      Planetary orbit data taken from <a href='https://nssdc.gsfc.nasa.gov/planetary/factsheet/' target='_blank'>https://nssdc.gsfc.nasa.gov/planetary/factsheet/</a>
      <br>
      Created by <a href='https://blogs.ed.ac.uk/johngodlee/' target='_blank'>John L. Godlee</a>
      <br>
      Source code on <a href='https://github.com/johngodlee/planetary_orbit_shiny' target='_blank'>Github</a>
      "),
      easyClose = TRUE
    ))
  })
}

shinyApp(ui, server)
