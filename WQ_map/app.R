
library(shiny)
library(leaflet)

ui <- fluidPage(

    titlePanel("Water Quality Sampling Map"),
        mainPanel(
           leafletOutput("map")
        )
    
)

server <- function(input, output) {

    output$map <- renderLeaflet({
        leaflet() %>%
          setView(-84.748501, 39.572083, zoom = 13) %>%
          addProviderTiles(providers$OpenStreetMap)
    })
}

shinyApp(ui = ui, server = server)
