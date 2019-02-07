######################################################
##### -- MPA iNaturalist Explorer R Shiny App -- #####
######################################################
################################
##### -- User Interface -- #####
################################

##### Load packages
library(shiny)
library(dplyr)
library(tidyr)
library(leaflet)
library(rgdal)
library(shinydashboard)
library(DT)
library(highcharter)
library(plotly)
library(collapsibleTree)

header<-dashboardHeader(title="MPA iNaturalist Explorer", titleWidth = 300)

body<-dashboardBody(
  
  tags$head(
    # Include custom CSS
    includeCSS("style.css"),
    includeScript("gomap.js")
  ),
  
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
  
  fluidRow(
    column(width = 6,
           box(width = NULL, solidHeader = TRUE,
               leafletOutput("mpa_map", height = 900)      
           )
    ),
    column(width = 6,
           fluidRow(height = 50, 
                    wellPanel(p("Select Marine Protected Area from the map: ", style="display:inline"), textOutput("selected_mpa"))
           ),
           
           fluidRow(
           tabBox(width = 12,
             # The id lets us use input$tabset1 on the server to find the current tab
             id = "tabset1", height = 840,
             tabPanel("Overview",
                      
                      fluidRow(
                        column(width = 6,
                               highchartOutput("observations_donut", height = 360)
                               ),
                        column(width = 6,
                               highchartOutput("observers_donut", height = 360)
                        )                        
                        ),
                      
                      br(),
                      
                      br(),

                      fluidRow(
                        column(width = 12,
                               highchartOutput("taxa_donut", height = 360)
                        )
                      )                      
                      ),
             tabPanel("Temporal trends", 
                      
                      fluidRow(
                        column(width = 12, 
                               
                               plotlyOutput("iNat_observations_plot", height = 350),
                               
                               br(),
                               
                               plotlyOutput("iNat_species_plot", height = 390)
                               
                               
                               )
                        
                        
                      )
                  ),
             
             tabPanel("Taxonomic breakdown", 
                      
                      fluidRow(
                        column(width = 12, 
                               
                               collapsibleTreeOutput("taxon_tree", height = 650)
                               
                        )
                        
                      ),
                      
                      fluidRow(
                        column(width = 12, 
                               sliderInput("time_range", "Temporal range:",
                                           min = 2000, max = 2018,
                                           timeFormat = TRUE,
                                           value = c(2000, 2018), step = 1,
                                           sep = "",
                                           width = 900
                               )
                        )
                      )
             ),
             
             tabPanel("Observer behavior", 
                      
                      fluidRow(
                        column(width = 6, 
                               
                               plotlyOutput("visit_observations_histogram", height = 350)
                               
                        ),
                        
                        column(width = 6, 
                               
                               plotlyOutput("visit_species_histogram", height = 350)
                               
                        )
                        
                      ),
                      
                      fluidRow(
                        column(width = 6, 
                               
                               plotlyOutput("visit_time_histogram", height = 350)
                               
                        ),
                        
                        column(width = 6, 
                               
                               plotlyOutput("visit_distance_histogram", height = 350)
                               
                        )
                        
                      )
             )  
           )
        )
    )
  )
)

dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)
