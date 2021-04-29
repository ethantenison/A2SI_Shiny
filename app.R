# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# -----------Libraries----------- #
# ------------------------------- #
# ------------------------------- #

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
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

# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# ------- Reference Data -------- #
# ------------------------------- #
# ------------------------------- #

austin_map <- readRDS("./data/austin_composite.rds")
austin_map <- as.data.frame(austin_map)
austin_map <- st_as_sf(austin_map)
austin_map <-
    st_transform(austin_map, "+proj=longlat +ellps=WGS84 +datum=WGS84")

austin_map <-
    austin_map %>% filter(
        var == "COMPOSITE (v1)" |
            var == "Air toxics cancer risk" |
            var == "Air toxics respiratory hazard index" |
            var == "Percentile for Diesel particulate matter level in air" |
            var == "Percentile for Ozone level in air" |
            var == "Percentile for PM2.5 level in air" |
            var == "Percentile for % people of color" |
            var == "Percentile for % low-income"
    )


var_choices <- unique(austin_map$var)

# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# ----------Dashboard UI--------- #
# ------------------------------- #
# ------------------------------- #


ui = dashboardPage(
    header = dashboardHeader(title = "A2SI"),
    sidebar = dashboardSidebar(useShinyjs(),
                               sidebarMenu(
                                   menuItem(
                                       "A2SI Data",
                                       tabName = "data",
                                       icon = icon("project-diagram")
                                   ),
                                   conditionalPanel(
                                       condition = "input.tabs == 'data'"
                                   )
                                   
                               )),
    body = dashboardBody(tabItems(tabItem(
        tabName = "data",
        fluidRow(column(width = 6,
                        box(title = "Austin Area Map",
                            width = 12,
                            status = "info",
                            background = "olive",
                            gradient = TRUE,
                            leafletOutput("bg", height = 700),
                            selectInput("var", "Variable", choices = var_choices)
                            
                        )),
                 column(width = 6,
                        box(title = "A2SI Indicators",
                            width = 12,
                            status = "danger",
                            background = "black",
                            gradient = TRUE,
                            DT::dataTableOutput("table")
                            
                        )))
    )))
    
)






# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# --------Dashboard Server------- #
# ------------------------------- #
# ------------------------------- #

server <- function(input, output, session) {
    #create the map
    output$bg <- renderLeaflet({
        leaflet(austin_map) %>%
            setView(lng = -97.74,
                    lat = 30.28,
                    zoom = 10)  %>%
            addProviderTiles(providers$Esri.WorldGrayCanvas)
    })
    
    
    
    variable <- reactive({
        austin_map %>% dplyr::filter(var == input$var)
    })
    
    observe({
        leafletProxy("bg", data = variable()) %>%
            clearShapes() %>%
            clearControls() %>%
            addPolygons(
                color = "#444444",
                weight = 1,
                smoothFactor = 0.5,
                opacity = 1.0,
                fillOpacity = 0.5,
                fillColor = ~ colorQuantile("YlOrRd", variable()$value)(variable()$value),
                highlightOptions = highlightOptions(
                    color = "white",
                    weight = 2,
                    bringToFront = TRUE
                ),
                label = ~ paste0(variable()$value)
            )
    })
    
}



# ------------------------------- #
# ------------------------------- #
# -----Run the application------- #
# ------------------------------- #
# ------------------------------- #
shinyApp(ui = ui, server = server)
