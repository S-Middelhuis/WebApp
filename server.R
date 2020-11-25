# Title:   Web App Server
# Name:    server.R
# Author:  Stijn Middelhuis
# Project: Regulate bed occupation in the clinic
#
# Description:
# This file contains the functions to set up the server
# of the Planning Web Application. The server reads the
# provided input from the UI, observes user actions and
# runs the necessary functions accordingly and returns 
# the requested output to the UI.

server <- function(input, output, session) {
  source("functions.R")
  
  ### Create reactive plots and tables ###
  plot  <- reactiveValues(main=NULL, layer1=NULL)
  table <- reactiveValues(info=NULL, date=NULL)
  
  ### Run main script ###
  observe({
    deps <- input$checkGroup
    data <- startScript(deps)
    
    # Plot results
    plot$main <- plot_results(data)
  })
  
  # Set button values
  refresh <- reactiveValues(timer=reactiveTimer(Inf))
  
  observeEvent(input$btn_terminate, {
      refresh$timer <- reactiveTimer(Inf) 
      output$check <- renderText("")
  })
  
  observeEvent(input$btn_auto, {
      refresh$timer <- reactiveTimer(600000) 
      output$check <- renderText("Auto refresh enabled")
  })
  
  
  ### Reload data ###
  # Automatic refresh
  observe({
      refresh$timer()
      data <- reload(input$checkGroup)
      
      # Plot results
      plot$main <- plot_results(data)
      
      # Read tables
      IC   <- read.csv('../Data/exports/overzichtIC.csv')
      SPEC <- read.csv('../Data/exports/overzichtSPEC.csv')
      IC   <- IC[,-c(1)]
      n    <- which( as.Date(IC$Datum) == floor_date(Sys.Date()+6, "week")+1 )
      
      colnames(SPEC)[1] <- "Datum"
      
      # Observe action button
      observe({
        print("render")
        output$plot <- renderPlot({ plot$main }, width = "auto", height = "auto")
        
        # Render tables in tabs
        output$wachtlijst <- renderTable(read.csv('../Data/exports/wachtlijst.csv'))
        output$huidig     <- renderTable(read.csv('../Data/exports/current.csv'))
        output$overzichtSPEC <- renderTable(SPEC)
        output$overzichtIC_1 <- renderTable(IC[1:n-1,])
        output$overzichtIC_2 <- renderTable(IC[n:nrow(IC),])
    })
  })
  
  # Manual refresh
  observeEvent(input$btn_refresh, {
    deps <- input$checkGroup
    data <- reload(deps)
    
    # Plot results
    plot$main <- plot_results(data)
     
    # Read tables
    IC   <- read.csv('../Data/exports/overzichtIC.csv')
    SPEC <- read.csv('../Data/exports/overzichtSPEC.csv')
    IC   <- IC[,-c(1)]
    n    <- which( as.Date(IC$Datum) == floor_date(Sys.Date()+5, "week")+1 )
    colnames(SPEC)[1] <- "Datum"
    
    # Observe action button
    observe({
      print("render")
      output$plot <- renderPlot({ plot$main }, width = "auto", height = "auto")
      
      # Render tables in tabs
      output$wachtlijst <- renderTable(read.csv('../Data/exports/wachtlijst.csv'))
      output$huidig     <- renderTable(read.csv('../Data/exports/current.csv'))
      output$overzichtSPEC <- renderTable(SPEC)
      output$overzichtIC_1 <- renderTable(IC[1:n-1,])
      output$overzichtIC_2 <- renderTable(IC[n:nrow(IC),])
    })
  })
  
  # Render table of clicked date
  observeEvent(input$plot_click, {
      table$date <- as.character(as.Date(round(as.numeric(input$plot_click$x), digits = 0), origin = "1970-01-01"))
      output$date <- renderText(table$date)
      table$info <- getDayPlanning(trunc(round(as.numeric(input$plot_click$x), digits = 0)))
      output$info <- renderTable(table$info)
  })
}