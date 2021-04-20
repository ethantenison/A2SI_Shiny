

library(shiny)
library(shinydashboard)
library(V8)
library(rintrojs)
library(shinyjs)
library(RColorBrewer)
library(dplyr)
library(readr)
library(tidyr)
library(leaflet)
library(leaflet.extras)
library(sf)
library(htmltools)
library(shinyWidgets)
library(ggplot2)
library(rsconnect)


austin_map <- readRDS("./data/austin_composite.rds")
austin_map <- as.data.frame(austin_map)
austin_map <- st_as_sf(austin_map)
austin_map <-
    st_transform(austin_map, "+proj=longlat +ellps=WGS84 +datum=WGS84")


ui <- fluidPage(mainPanel(#this will create a space for us to display our map
    leafletOutput(outputId = "bg")))


server <- function(input, output, session) {
    #create the map
    output$bg <- renderLeaflet({
        leaflet(austin_map) %>%
            setView(lng = -99,
                    lat = 45,
                    zoom = 2)  %>% #setting the view over ~ center of North America
            addPolygons(
                color = "#444444",
                weight = 1,
                smoothFactor = 0.5,
                opacity = 1.0,
                fillOpacity = 0.5,
                fillColor = ~ colorQuantile("YlOrRd", composite_v1)(composite_v1),
                highlightOptions = highlightOptions(
                    color = "white",
                    weight = 2,
                    bringToFront = TRUE
                )
            )
    })
    
}

# Run the application
shinyApp(ui = ui, server = server)
