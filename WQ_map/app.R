
library(shiny)
library(leaflet)
library(sf)
ui <- fluidPage(

    titlePanel("Water Quality Sampling Map"),
    sidebarLayout(
      sidebarPanel(
        checkboxInput("showPoints", "Show Points", value = TRUE)
      ),
        mainPanel(
           leafletOutput("map")
        )
    
    )
)

server <- function(input, output, session) {
    sample_points <- reactive({
      st_read("sample_points.geojson")
    })
    
    filteredData <- reactive({
      if (input$showPoints) {
        data <- sample_points()
        data$lon <- as.numeric(st_coordinates(data)[, 1])
        data$lat <- as.numeric(st_coordinates(data)[, 2])
        return(data)
      } else {
        return(NULL)
      }
    })
    
    output$map <- renderLeaflet({
        leaflet() %>%
          setView(-84.748501, 39.572083, zoom = 13) %>%
          addProviderTiles(providers$OpenStreetMap)
    })
    
    observe({
      data <- filteredData()
      if (!is.null(data)) {
        leafletProxy("map", data = data) %>%
          clearMarkers() %>%
          addCircleMarkers(~lon, ~lat, radius = 1, popup = ~as.character(name))
      } else {
        leafletProxy("map") %>%
          clearMarkers()
      }
    })
}

# Server
server <- function(input, output, session) {
  # Load the GeoJSON data
  points_data <- reactive({
    st_read("points.geojson")
  })
  
  # Reactive expression for the data to be shown or hidden
  filteredData <- reactive({
    if (input$showPoints) {
      data <- points_data()
      data$lon <- as.numeric(st_coordinates(data)[, 1])
      data$lat <- as.numeric(st_coordinates(data)[, 2])
      return(data)
    } else {
      return(NULL)
    }
  })
}
shinyApp(ui = ui, server = server)
