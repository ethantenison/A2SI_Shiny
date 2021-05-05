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
library(plotly)

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


austin_map <- austin_map %>% filter(GEOID_ != 480559601011 & GEOID_ != 480559601012 & GEOID_ != 484910203012)

var_choices <- unique(austin_map$var)

# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# ----------Dashboard UI--------- #
# ------------------------------- #
# ------------------------------- #


ui = dashboardPage(
    skin = "black-light",
    header = dashboardHeader(title = tagList(
        span(class = "logo-lg", tags$img(src = 'images/logo_skinny.png', width =
                                             '50%')),
        img(src = 'images/arrow.png', width = '150%')
    )),
    sidebar = dashboardSidebar(
        useShinyjs(),
        collapsed = TRUE,
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
    body = dashboardBody(tags$head(tags$style(
        HTML('
.box {margin: 20px;}')
    )),
tabItems(
    tabItem(tabName = "data",
            
            fluidRow(
                column(width = 6,
                       style='padding:20px;',
                       offset = 0,
                       fluidRow(
                           box(
                               title = "Austin Area Map",
                               width = 12,
                               solidHeader = TRUE,
                               leafletOutput("bg", height = 600),
                               selectInput("var", "Select a Variable", choices = var_choices, selected = "Composite Climate Hazard Exposure")
                               
                           )
                       )),
                column(width = 6,
                       offset = 0,
                       style='padding:20px;',
                       fluidRow(
                           div(
                               id = "logo",
                               style = "padding-left: 20px !important;",
                               HTML(
                                   '<center><img src="images/AASI_logo_v1b-01.png" width="300"></center>'
                               )
                           )
                       ),
                       fluidRow(
                           column(
                               width = 6,
                               offset = 0,
                               style='padding:20px; padding-left: 0px;',
                               box(
                                   title = "Variable Distribution",
                                   width = 12,
                                   solidHeader = TRUE,
                                   plotlyOutput("violin")
                                   
                               )
                           ),
                           column(
                               width = 6,
                               offset = 0,
                               style='padding:20px;padding-left: 0px;',
                               box(
                                   title = "Variable breakdown by Demographic Indicators",
                                   width = 12,
                                   solidHeader = TRUE,
                                   plotlyOutput("barplot")
                                   
                               )
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
    
    
    #Color Palette for Map
    pal <- reactive({
        colorNumeric(palette = "viridis",
                     n = 10,
                     domain = variable()$value)
    })
    
    #Map attributes to display
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
                    "People of Color (%): ",
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
    
    #Violin Plot of Variable Selected
    output$violin <- renderPlotly({
        plot_ly(
            y = ~ variable()$value,
            type = 'violin',
            box = list(visible = T),
            color = I("#29AF7F"),
            meanline = list(visible = T),
            x0 = input$var,
            hoverinfo = "none"
        )  %>%
            layout(yaxis = list(title = "",
                                zeroline = F))
        
        
        
    })
    
    #Data for barplot
    bar <- reactive({
        bar <-
            austin_map %>%
            dplyr::filter(var == input$var) %>%
            mutate(
                `> 50% People of Color` = if_else(`% people of color` >= 0.5, 1, 0),
                `> 50% Low Income` = if_else(`% low-income` >= 0.5, 1, 0)
            )
        
        total_av <- mean(bar$value)
        
        poc <- bar %>% filter(`> 50% People of Color` == 1)
        poc_av <- mean(poc$value)
        
        lowincome <- bar %>% filter(`> 50% Low Income` == 1)
        lowincome_av <- mean(lowincome$value)
        
        
        bar_to_plotly <-
            data.frame(
                y = c(total_av, poc_av, lowincome_av),
                x = c(
                    "Austin Average",
                    "> 50% People of Color",
                    "> 50% Low Income"
                )
            )
        
        bar_to_plotly
    })
    
    
    #Plotly Barplot
    output$barplot <- renderPlotly({
        plot_ly(
            x = bar()$x,
            y = bar()$y,
            color = I("#29AF7F"),
            type = 'bar',
            box = list(visible = T),
            meanline = list(visible = T)
            
        )
        
    })
    
    
    
    
    
    
}



# ------------------------------- #
# ------------------------------- #
# -----Run the application------- #
# ------------------------------- #
# ------------------------------- #
shinyApp(ui = ui, server = server)
