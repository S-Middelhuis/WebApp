# Set up server
server <- function(input, output, session) {
  source("functions.R")
  
  # Create reactive plots and tables
  plot <- reactiveValues(main=NULL, layer1=NULL)
  table <- reactiveValues(info=NULL, date=NULL)
  
  # Run main script
  observe({
    deps = input$checkGroup
  
    data <- startScript(deps)
    
    # Legend and plot color
    Legenda <- factor(c("Planning", "Meest recente mutaties"))
    palette <- c(rgb(120/255, 120/255, 120/255), rgb(20/255, 150/255, 40/255), rgb(60/255, 230/255, 90/255))
    
    # Plot results
    plot$main <- ggplot(data = data, aes(x=dates)) +    
      geom_bar(stat="identity", aes(y=prob.p, fill = "Recente mutaties")) +
      geom_bar(stat="identity", aes(y=prob.r, fill = "Planning")) +
      geom_bar(stat="identity", aes(y=prob.c, fill = "Huidige bezetting")) +
      scale_fill_manual("Legenda", values = palette) + 
      #geom_line(aes(x=data$dates, y=data$max, color = "Max capaciteit")) +  
      #scale_color_manual("Legenda", values = "red") +
      xlab("Datum") +
      ylab("Bedden bezetting") +
      theme(legend.key = element_blank(),
            legend.title = element_blank())
  })
  # Observe action button
  observe({
    print("render")
    output$plot <- renderPlot({ plot$main }, width = "auto", height = "auto")
  })
  
  
  refresh <- reactiveValues(timer=reactiveTimer(Inf))
  
  observeEvent(input$btn_terminate, {
      refresh$timer <- reactiveTimer(Inf) 
      output$check <- renderText("")
  })
  observeEvent(input$btn_auto, {
      refresh$timer <- reactiveTimer(10000) 
      output$check <- renderText("Auto refresh enabled")
  })
  ### Reload data ###
  observe({
      refresh$timer()
      
      data <- reload(input$checkGroup)
      
      # Legend and plot color
      Legenda <- factor(c("Planning", "Meest recente mutaties"))
      palette <- c(rgb(120/255, 120/255, 120/255), rgb(20/255, 150/255, 40/255), rgb(60/255, 230/255, 90/255))
      
      # Plot results
      plot$main <- ggplot(data = data, aes(x=dates)) +    
        geom_bar(stat="identity", aes(y=prob.p, fill = "Recente mutaties")) +
        geom_bar(stat="identity", aes(y=prob.r, fill = "Planning")) +
        geom_bar(stat="identity", aes(y=prob.c, fill = "Huidige bezetting")) +
        scale_fill_manual("Legenda", values = palette) + 
        #geom_line(aes(x=data$dates, y=data$max, color = "Max capaciteit")) +  
        #scale_color_manual("Legenda", values = "red") +
        xlab("Datum") +
        ylab("Bedden bezetting") +
        theme(legend.key = element_blank(),
              legend.title = element_blank())
      
      # Read tables
      IC         <- read.csv('Data/overzichtIC.csv')
      SPEC       <- read.csv('Data/overzichtSPEC.csv')
      IC   <- IC[,-c(1)]
      n    <- which( as.Date(IC$Datum) == floor_date(Sys.Date()+6, "week")+1 )
      
      colnames(SPEC)[1] <- "Datum"
      
      # Observe action button
      observe({
        print("render")
        output$plot <- renderPlot({ plot$main }, width = "auto", height = "auto")
        # Render tables in tabs
        output$wachtlijst <- renderTable(read.csv('Data/wachtlijst.csv'))
        output$huidig     <- renderTable(read.csv('Data/current.csv'))
        output$overzichtSPEC  <- renderTable(SPEC)
        output$overzichtIC_1 <- renderTable(IC[1:n-1,])
        output$overzichtIC_2 <- renderTable(IC[n:nrow(IC),])
    })
  })

  
  
  observeEvent(input$btn_refresh, {
    deps <- input$checkGroup
    data <- reload(deps)
    
    # Legend and plot color
    Legenda <- factor(c("Planning", "Meest recente mutaties"))
    palette <- c(rgb(120/255, 120/255, 120/255), rgb(20/255, 150/255, 40/255), rgb(60/255, 230/255, 90/255))
    
    
    # Plot results
    plot$main <- ggplot(data = data, aes(x=dates)) +    
      geom_bar(stat="identity", aes(y=prob.p, fill = "Recente mutaties")) +
      geom_bar(stat="identity", aes(y=prob.r, fill = "Planning")) +
      geom_bar(stat="identity", aes(y=prob.c, fill = "Huidige bezetting")) +
      scale_fill_manual("Legenda", values = palette) + 
      #geom_line(aes(x=data$dates, y=data$max, color = "Max capaciteit")) +  
      #scale_color_manual("Legenda", values = "red") +
      xlab("Datum") +
      ylab("Bedden bezetting") +
      theme(legend.key = element_blank(),
            legend.title = element_blank())
     
      # Read tables
      IC         <- read.csv('Data/overzichtIC.csv')
      SPEC       <- read.csv('Data/overzichtSPEC.csv')
      IC <- IC[,-c(1)]
      n    <- which( as.Date(IC$Datum) == floor_date(Sys.Date()+5, "week")+1 )
      colnames(SPEC)[1] <- "Datum"
    
    # Observe action button
    observe({
      print("render")
      output$plot <- renderPlot({ plot$main }, width = "auto", height = "auto")
      
      # Render tables in tabs
      output$wachtlijst <- renderTable(read.csv('Data/wachtlijst.csv'))
      output$huidig     <- renderTable(read.csv('Data/current.csv'))
      output$overzichtSPEC <- renderTable(SPEC)
      output$overzichtIC_1 <- renderTable(IC[1:n-1,])
      output$overzichtIC_2 <- renderTable(IC[n:nrow(IC),])
      
    })
  })
  
  
  # Render table of clicked date
  observeEvent(input$plot_click, {
      table$date <-as.character(as.Date(round(as.numeric(input$plot_click$x), digits = 0), origin = "1970-01-01"))
      output$date <- renderText(table$date)
      table$info <- getDayPlanning(trunc(round(as.numeric(input$plot_click$x), digits = 0)))
      output$info <- renderTable(table$info)
  })
}