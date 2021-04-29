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
library(DT)

# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# ------- Reference Data -------- #
# ------------------------------- #
# ------------------------------- #
options(scipen = 999)

austin_map <- readRDS("./data/austin_composite.rds")
austin_map <- as.data.frame(austin_map)
austin_map <- st_as_sf(austin_map)
austin_map <-
    st_transform(austin_map, "+proj=longlat +ellps=WGS84 +datum=WGS84")


var_choices <- unique(austin_map$var)

# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# ----------Dashboard UI--------- #
# ------------------------------- #
# ------------------------------- #


ui = dashboardPage(
    header = dashboardHeader(title = tagList(
        span(class = "logo-lg", tags$img(src = 'images/logo_skinny.png', width =
                                             '50%')),
        img(src = "https://image.flaticon.com/icons/svg/204/204074.svg")
    )),
    sidebar = dashboardSidebar(
        useShinyjs(),
        sidebarMenu(
            menuItem(
                "A2SI Data",
                tabName = "data",
                icon = icon("project-diagram")
            ),
            conditionalPanel(condition = "input.tabs == 'data'"),
            menuItem(
                "About Research",
                tabName = "about",
                icon = icon("question")
            ),
            conditionalPanel(condition = "input.tabs == 'about'")
            
        )
    ),
    body = dashboardBody(tabItems(
        tabItem(tabName = "data",
                
                fluidRow(
                    column(width = 6,
                           fluidRow(
                               box(
                                   title = "Austin Area Map",
                                   width = 12,
                                   status = "info",
                                   solidHeader = TRUE,
                                   leafletOutput("bg", height = 600),
                                   selectInput("var", "Variable", choices = var_choices)
                                   
                               )
                           )),
                    column(width = 6,
                           fluidRow(
                               div(
                                   id = "logo",
                                   style = "padding-left: 20px !important;",
                                   HTML('<center><img src="images/AASI_logo_v1b-01.png" width="400"></center>')
                               )
                           ),
                           fluidRow(
                               box(
                                   title = "A2SI Indicators",
                                   width = 12,
                                   status = "info",
                                   solidHeader = TRUE
                                   # DT::dataTableOutput("table", width = "20%")
                                   
                               )
                           ))
                )),
        tabItem(tabName = "about",
                fluidRow())
    ))
    
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
    
    pal <- reactive({
        colorNumeric(palette = "viridis",
                     n = 10,
                     domain = variable()$value)
        
        
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
                fillColor = ~ pal()(variable()$value),
                highlightOptions = highlightOptions(
                    color = "white",
                    weight = 2,
                    bringToFront = TRUE
                ),
                label = ~ paste0(
                    variable()$var,
                    ": ",
                    format(variable()$value, digits = 1)
                ),
                
                popup =  ~ paste0(
                    "<h5/><b>",
                    variable()$var,
                    ": ",
                    format(variable()$value, digits = 1),
                    "<h6/>",
                    "Census Block Group: ",
                    GEOID_,
                    "<h6/>",
                    "Total population: ",
                    format(variable()$`Total population`, big.mark = ","),
                    "<h6/>",
                    "People of COlor (%): ",
                    format(variable()$`% people of color`, digits = 1),
                    "<h6/>",
                    "Low Income (%): ",
                    format(variable()$`% low-income`, digits = 1)
                )
            ) %>%
            addLegend(
                "bottomright",
                pal = pal(),
                values = ~ variable()$value,
                title = input$var
            )
    })
    
    
    
}



# ------------------------------- #
# ------------------------------- #
# -----Run the application------- #
# ------------------------------- #
# ------------------------------- #
shinyApp(ui = ui, server = server)
