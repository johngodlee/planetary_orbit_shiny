library(shiny)
library(leaflet)
library(dplyr)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}", 
    ".leaflet .legend {
      line-height: 25px;
      font-size: 25px;
      border-radius: 15px;
      padding: 20px;
      opacity: 0.8;
    }",
    ".leaflet .legend i{
      width: 30px;
      height: 30px;
    }",
    "#panel {background-color: white; 
      padding: 20px;
      border-radius: 15px;
      opacity: 0.8;}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(id="panel", top = 10, right = 10,
    sliderInput("diam", "Diameter of model Sun (cm)", 1, 1000,
      value = 5, step = 1),
    tableOutput("dist"),
    textOutput("text")
  )
)

server <- function(input, output, session) {
  
  planet_names <- c("Mercury", "Venus", "Earth", "Mars", "Jupiter", "Saturn", "Uranus", "Neptune")
  
  # Planet distances from Sun (km)
  dMer <- 57.9*10^6
  dVen <- 108.2*10^6
  dEar <- 149.6*10^6
  dMar <- 227.9*10^6
  dJup <- 778.6*10^6
  dSat <- 1433.5*10^6
  dUra <- 2872.5*10^6
  dNep <- 4495.1*10^6
  
  dist <- c(dMer, dVen, dEar, dMar, dJup, dSat, dUra, dNep)
  
  # Diameter of Sun
  DSun <- 1.39*10^6
  
  # Dataframe - Edinburgh default location
  dist_df <- data.frame(planet_names, dist, x = -3.169, y = 55.946)
  
  # Reactive expression - scaling distances to diameter of Sun model
  dist_scale_df <- reactive({
    dist_df$dist_scale <- dist * ((input$diam / 10^5) / DSun) * 1000
    dist_df$planet_names <- factor(dist_df$planet_names, levels = planet_names)
    return(dist_df[order(dist_df$dist_scale),])
  })
  
  # Static map elements
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
        options = providerTileOptions(noWrap = TRUE)) %>%
      setView(lng = mean(dist_df$x), lat = mean(dist_df$y), zoom = 15)
  })
  
  pal <- colorFactor(
    palette = 'Dark2',
    domain = dist_df$planet_names
  )
  
  # Dynamic map elements
  observe({
    leafletProxy("map") %>%
      clearShapes() %>%
      clearControls() %>%
      addCircles(data = dist_scale_df(), lng = ~x, lat = ~y, radius = ~dist_scale,
        weight = 5, color = ~pal(planet_names), fillColor = "#319E88", fillOpacity = 0.01) %>%
      addCircleMarkers(lng = -3.169, lat = 55.946, radius = 2, color = "black", stroke = FALSE, fillOpacity = 1) %>%
      addLegend(data = dist_scale_df(), "bottomright", pal = pal, values = ~planet_names,
        title = "Planet orbit",
        opacity = 1
      )
    })
  
  changeSciNot <- function(n) {
    output <- format(n, scientific = TRUE) #Transforms the number into scientific notation even if small
    output <- sub("e", "x10^", output) #Replace e with 10^
    output <- sub("\\+0?", "", output) #Remove + symbol and leading zeros on expoent, if > 1
    output <- sub("-0?", "-", output) #Leaves - symbol but removes leading zeros on expoent, if < 1
    output <- gsub("\\^", "<sup>", output)
    output <- paste0(output, "</sup>")
    output
  }
  
  # Table output
  output$dist <- renderTable(dist_scale_df() %>%
      mutate(dist_scale = dist_scale,
        dist = changeSciNot(dist)) %>%
      dplyr::select(Planet = planet_names, `Real Distance (km)` = dist, `Scaled Distance (m)` = dist_scale),
    sanitize.text.function = function(x) x)
  
  output$text <- renderText(input$map_click[[1]])
}

shinyApp(ui, server)

