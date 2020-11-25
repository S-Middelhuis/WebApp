# Title:   Web App User Interface
# Name:    ui.R
# Author:  Stijn Middelhuis
# Project: Regulate bed occupation in the clinic
# file:    ui.R
#
# Description:
# This file contains the set up of the user interface
# of the Planning Web Application. The UI interface
# consists of a html fluid page on which several html
# items are placed with a specific function and output.
# The input for the different items are requested and 
# returned by the server.

ui <- fluidPage(
  titlePanel( "Visualisatie bezetting beddenhuis na OK" ),
  mainPanel(
    tabsetPanel(type = "tab",
                tabPanel("PlanApp", 
                         fluidRow(
                         column(10,
                                plotOutput("plot", click = "plot_click")),
                         column(2,
                                checkboxGroupInput("checkGroup", 
                                                   h3("Reserverings afdelingen"), 
                                                   choices = list("2A" = 1, 
                                                                  "2B" = 2, 
                                                                  "2C" = 3,
                                                                  "2D" = 4),
                                                   selected = c(3,4)))
                         ),
                          
                
                         actionButton( "btn_refresh", "Refresh" ),
                         actionButton( "btn_auto", "Auto refresh" ),
                         actionButton( "btn_terminate" , "Stop auto refresh" ),
                         span( textOutput("check"), style = "color:green" ),
                         fluidRow(
                           br(),
                           tableOutput( "patient" )
                         ),
                         textOutput( "date" ),
                         tableOutput( "info" ),
                         
                         fluidRow(
                           column(3,
                                  h4( "IC/MC Aantallen" ),
                                  tableOutput( "overzichtIC_1" ),
                                  tags$i( "Volgende week" ),
                                  tableOutput( "overzichtIC_2" )
                                  ),
                           column(3,
                                  h4( "Specialisme Aantallen" ),
                                  tableOutput( "overzichtSPEC" )
                                  )
                                  )
                         ),
                tabPanel("Huidig", 
                         tableOutput( "huidig" )
                         ),
                tabPanel("Wachtlijst", 
                         tableOutput( "wachtlijst" )
                         )
                
                
    )
  )
)

