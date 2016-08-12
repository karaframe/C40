

library(shiny)
library(dplyr)
library(leaflet)
library(shinydashboard)
library(plotly)
library(shinyjs)
library(V8)
# library(shinyIncubator)
# install.packages("V8")

# Clear all objects
rm(list = ls(all = TRUE))

############################################################

jscode <- "shinyjs.refresh = function() { history.go(0); }"

ui <- dashboardPage(skin = "red",
  dashboardHeader(title = "GPC GHG Emissions"),

  dashboardSidebar(
    width = 250,
    paste("Time:",Sys.time()),
                   sidebarMenu(
                     selectInput(
                       "name", "City name",
                       c(
                         "Buenos Aires" = "Buenos Aires",
                         "London" = "London",
                         "New York" = "New York",
                         "Milano" = "Milano"
                       )
                     ),
                     
                     
                    #  fileInput('file1', 'choose xlsm File',
                    #            accept = c(".xlsm")),
                    #  
                    # fluidRow(
                    #   column (3,offset = 1,
                    #  actionButton("goButton", strong("Process xlsm file"), icon("cog", lib = "glyphicon"), 
                    #               style="color: #000000; background-color: #ffff7f ; border-color: #ffff7f"), # width = 150
                    #  # actionButton("resetButton", "clear", icon("paper-plane"),
                    #  #              style="color: #fff; background-color: #ff4c4c; border-color: #2e6da4"),
                    #  tableOutput('contents')
                    #  )),
                    
                    
          
                     menuItem("Map", tabName = "CITY", icon = icon("th")),
                     menuItem("Sectors", tabName = "Sect", icon = icon("th")),
                     menuItem("Sub Sectors", tabName = "Subsect", icon = icon("th"))
                    
                    
                    # fluidRow(
                    #   column (3,offset = 1,
                    #           br(), #hr()
                    #           useShinyjs(),
                    #           extendShinyjs(text = jscode),
                    #           actionButton("refresh", "refresh", icon("paper-plane"), 
                    #                        style="color: #fff; background-color: #7fbf7f; border-color: #2e6da4", width = 150)
                    #   ))
                    
                   )),

  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
    
    # First tab content
    tabItem(tabName = "CITY",
            fluidRow(
              tabBox(
                height = 750, width = 950, selected = tags$b("City Location"),
                tabPanel(
                  tags$b("City Location"), leafletOutput('myMap', height = 650, width = 750)
                )
              )
            )),
    
    
    
    # Second tab content
    tabItem(tabName = "Sect",
            fluidRow(
              tabBox(
                height = 750, width = 950, selected = tags$b("Data"),
                tabPanel(
                  tags$b("Data"), tableOutput('sectors')
                ),
                tabPanel(
                   tags$b("Summary plot"), 
                   box(title = " ", height = 550, width = 650, plotlyOutput("sectors_plot"))
                   )
              )
            )),



    tabItem(tabName = "Subsect",
            fluidRow(
              tabBox(
                height = 750, width = 950, selected = tags$b("Data"), 
                tabPanel(
                  tags$b("Data"), tableOutput('subsectors')
                ),
                tabPanel(
                        tags$b("Summary plot"),
                        column(3, plotlyOutput("subsectors_plot", height = "700px", width = "850px"))
                        )
              )
            ))
  ))
)




