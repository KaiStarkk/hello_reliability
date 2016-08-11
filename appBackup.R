#Load required libraries
library(shiny)
library(shinydashboard)
library(shinyjs)
library(matrixStats)
library(DT)

## ------------------------------
## UI functionality
## ------------------------------

ui <- dashboardPage(
  dashboardHeader(title = "Reliability Application"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Process Analysis", tabName = "process-analysis", icon = icon("dashboard"), badgeLabel = "beta", badgeColor = "purple"),
      menuItem("Settings", tabName = "settings", icon = icon("gears")),
      useShinyjs(),
      actionButton("helpButton", "Help", icon = icon("question-circle"), class="btn-info", onclick="showTour();")
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", href = "css/custom.css"),
      tags$link(rel = "stylesheet", href = "css/bootstrap-tour-standalone.min.css"),
      tags$head(tags$script(src="js/bootstrap-tour-standalone.min.js", type="text/javascript")),
      tags$head(tags$script(HTML('function showTour() {
                                 var tour = new Tour({
                                 steps: [
                                 {
                                 title: "Welcome",
                                 content: "Welcome to the Reliability Application"
                                 },
                                 {
                                 element: ".main-sidebar",
                                 title: "Navigation",
                                 content: "This navtray allows you to move between different sections of the application. The default section is Process Analysis."
                                 },
                                 {
                                 element: "#shiny-tab-process-analysis .col-sm-8",
                                 title: "Process Information",
                                 placement: "bottom",
                                 onShow: function(tour) {if($(window).width() < 750) {$(document.body).addClass("sidebar-collapse");$(document.body).removeClass("sidebar-open")}},
                                 content: "Information about the process baseline is shown in this panel. You can choose between chart and table view with the tab bar."
                                 },
                                 {
                                 element: "#shiny-tab-process-analysis .col-sm-4",
                                 title: "Adding Data",
                                 placement: "left",
                                 content: "Here you can add data to the process. The first tab allows manual addition of samples and groups, and the second facilitates batch uploading."
                                 }
                                 ],
                                 orphan: true,
                                 backdrop: true,
                                 storage: false
                                 });
                                 tour.init();
                                 tour.start();
                                 }',
                            type='text/javascript')))
      ),
    
    tabItems(
      
      tabItem(
        tabName = "process-analysis",
        h2("Process Analysis"),
        fluidRow(
          column(
            width = 8,
            tabBox(
              width = NULL,
              tabPanel(
                "SPC Chart",
                plotOutput("SPCChart",
                           click = "plot_click",
                           dblclick = "plot_dblclick",
                           hover = "plot_hover",
                           brush = "plot_brush")
              ),
              tabPanel(
                "Table View",
                DT::dataTableOutput('tableView')
              )
            )
          ),
          column(
            width = 4,
            tabBox(
              width = NULL,
              tabPanel(
                "Add Samples",
                p("Add sample groups one by one."),
                textInput("addSampleTextInput", "Sample Value"),
                actionButton("groupSampleActionButton", "Add Sample To Group", icon = icon("plus")),
                actionButton("addSampleActionButton", "Add Sample To Process", icon = icon("plus")),
                br(),br(),
                strong("Group:"),
                br(),
                DT::dataTableOutput('addGroupTable'),
                br(),
                actionButton("removeSamplesActionButton", "Remove Selected Samples From Group", icon = icon("minus")),
                actionButton("clearGroupActionButton", "Clear Group", icon = icon("times")),
                actionButton("addGroupActionButton", "Add Group To Process", icon = icon("plus"))
              ),
              tabPanel(
                padding = 20,
                "Upload Samples",
                p("Select a sample group in csv format."),
                fileInput('file1', '',
                          accept = c(
                            'text/csv',
                            'text/comma-separated-values',
                            'text/tab-separated-values',
                            'text/plain',
                            '.csv',
                            '.tsv'
                          )
                )
              )
            ),
            box(
              width = NULL,
              title = "Results",
              p(span(HTML("&alpha;"))," Value: 0.05"),
              textOutput("contents"),
              fluidRow(
                valueBoxOutput("cpkBox", width = 6),
                valueBoxOutput("totalAreaPercentageBox", width = 6)
              ),
              div(textOutput('good'),style="color:green"),
              div(textOutput('bad'),style="color:red"),
              textOutput('defPercentage')
            )
          )
        )
      ),
      
      tabItem(
        tabName = "settings",
        h2("Settings"),
        fluidRow(
          box(
            width = 12, height = 200, status = "danger", solidHeader = TRUE, title = "Danger Zone!",
            h3("Reset Grand Process"),
            p("This will delete all sample and group data. Please back up all required information before proceeding."),
            actionButton("reset-grand-process-actionButton", "Delete", class="btn-danger")
          )
        )
      )
      
    )
      )
      )

## ------------------------------
## Server functionality
## ------------------------------

server <- function(input, output) {
  
  ##Load in our helpder functions
  source("controlChecks.R")
  
  added <- NULL
  
  # By default, the file size limit is 5MB. It can be changed by
  # setting this option. Here we'll raise limit to 9MB.
  options(shiny.maxRequestSize = 9*1024^2)
  
  #Load in our big dataset (For now,generate a fake dataset )
  voltages <- as.data.frame(replicate(4, rnorm(10,mean=1.31,sd=0.05)))
  
  #Calculate X'' and R
  sampleMeans <- rowMeans(voltages)
  lineGraph <- reactiveValues(data=rowMeans(voltages))
  gpAve <- mean(sampleMeans)
  
  maxMinList <- rowRanges(as.matrix(voltages))
  sampleRanges <- maxMinList[,2]-maxMinList[,1]
  rangeAve<- mean(sampleRanges)
  
  #Calculate UCL and LCL
  a2 <- 0.729
  ucl <- gpAve + a2*rangeAve
  lcl <- gpAve - a2*rangeAve
  
  #Determine the intervals for each zone. Store in a dataframe for easy reference later on
  zoneDistance <- (ucl - gpAve)/3
  
  cZones <- c(gpAve + zoneDistance, gpAve - zoneDistance)
  bZones <- c(gpAve + 2*zoneDistance, gpAve - 2*zoneDistance)
  aZones <- c(ucl, lcl)
  
  zoneDf <- data.frame(cZones,bZones,aZones)
  colnames(zoneDf) <- c('C', 'B', 'A')
  
  #Render the SPC Chart
  output$SPCChart <- renderPlot({
    
    xmin <- -length(lineGraph$data)/7
    
    #Plot X' with a clear centreline, dotted lines for each zone and a dashed line for UCL and LCL
    plot(lineGraph$data, type = "b", ylab = "Average of each sample", xlab = "Sample", xlim=c(xmin,length(lineGraph$data)),
         ylim = c(min(min(lineGraph$data),lcl)-0.01,max(max(lineGraph$data),ucl)+0.01), xaxt='n', ann=FALSE)
    
    #Plot the centreline
    abline(h=gpAve, lty=4, col="green")
    text(xmin*7/10, gpAve, labels="Process Average")
    
    #Plot LCL and UCL
    abline(h=lcl, lty=2, col="red")
    text(xmin*7/10, lcl, labels="LCL", pos=1)
    abline(h=ucl, lty=2, col="red")
    text(xmin*7/10, ucl, labels="UCL", pos=3)
    
    #Plot the zones
    abline(h=zoneDf$C[1], lty=3, col="blue")
    abline(h=zoneDf$C[2], lty=3, col="blue")
    
    abline(h=zoneDf$B[1], lty=3, col="blue")
    abline(h=zoneDf$B[2], lty=3, col="blue")
    
    text(xmin*7/10, gpAve + zoneDistance/2, labels="Zone C")
    text(xmin*7/10, gpAve - zoneDistance/2, labels="Zone C")
    
    text(xmin*7/10, gpAve + 1.5*zoneDistance, labels="Zone B")
    text(xmin*7/10, gpAve - 1.5*zoneDistance, labels="Zone B")
    
    text(xmin*7/10, gpAve + 2.5*zoneDistance, labels="Zone A")
    text(xmin*7/10, gpAve - 2.5*zoneDistance, labels="Zone A")
  })
  
  loadedGroup <- reactiveValues(data=data.frame(Point=numeric(0)))
  
  #Create eventhandlers for actionButtons
  
  #"Add sample to group" button
  observeEvent(input$groupSampleActionButton, {
    groupSampleContents <- as.numeric(as.character(input$addSampleTextInput))
    if(!is.null(groupSampleContents) && !is.na(groupSampleContents)) {
      loadedGroup$data <- rbind(data.frame(Point=as.numeric(as.character(loadedGroup$data[,1]))),data.frame(Point=groupSampleContents))
    }
  })
  
  #"Remove sample from group" button
  observeEvent(input$removeSamplesActionButton, {
    remaining <- data.frame(Point=loadedGroup$data[!rownames(loadedGroup$data) %in% input$addGroupTable_rows_selected,])
    loadedGroup$data <- remaining
  })
  
  #"Clear group" button
  observeEvent(input$clearGroupActionButton, {
    loadedGroup$data = data.frame(Point=data.frame(Point=numeric(0)))
  })
  
  addedSample <- reactiveValues(data=NULL)
  addedGroup <- reactiveValues(data=NULL)
  
  #"Add sample to process" button
  observeEvent(input$addSampleActionButton, {
    addSampleContents <- as.numeric(as.character(input$addSampleTextInput))
    if(!(is.null(addSampleContents) || is.na(addSampleContents))) {
      added <<- "sample"
      addedSample$data <- NULL
      addedSample$data <- addSampleContents
    } else {
      return(NULL)
    }
  })
  
  #"Add group to process" button
  observeEvent(input$addGroupActionButton, {
    if (!is.null(loadedGroup$data)) {
      added <<- "group"
      addedGroup$data <- NULL
      addedGroup$data <- data.frame(Point=as.numeric(as.character(loadedGroup$data[,1])))
    } else {
      return(NULL)
    }
  })
  
  #Render the DataTables
  output$tableView = DT::renderDataTable(
    data.frame(Mean=round(sampleMeans, digits = 3), Range=round(sampleRanges, digits = 3)),
    options = list(pageLength = 10, pagingType = "full", searching = FALSE)
  )
  
  output$addGroupTable <- DT::renderDataTable(server = TRUE,escape=F,options = list(paging = FALSE, searching = FALSE),{
    if (is.null(loadedGroup$data)) {
      data.frame(Point=numeric(0))
    } else {
      loadedGroup$data
    }
  })
  
  tableViewProxy <- DT::dataTableProxy('tableView')
  addGroupTableProxy <- DT::dataTableProxy('addGroupTable')
  
  output$contents <- renderText({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inSample <- addedSample$data
    
    inGroup <- addedGroup$data[,1]
    
    inFile <- input$file1
    
    if (!is.null(added) && added=="sample" && !(is.null(inSample) || length(inSample)!=1)) {
      added <<- NULL
      newSample <- inSample
    } else if (!is.null(added) && added=="group" && !(is.null(inGroup) || length(inGroup)<1)) {
      added <<- NULL
      newSample <- inGroup
      loadedGroup$data = data.frame(Point=numeric(0))
    } else if (is.null(added) && !is.null(inFile)) {
      inputData <- read.csv(inFile$datapath)
      newSample <- inputData[,2]
    } else {
      added <<- NULL
      return(NULL)
    }
    
    #Calculate the stats associated with the new sample
    sampleMean <- mean(newSample)
    sampleSize <- length(newSample)
    sampleRange <- diff(range(newSample))
    stDevOptions <- c(1,1.128,1.693,2.059,2.326,2.534,2.704,2.847,2.970,3.078)
    sampleStDev <- stDevOptions[sampleSize]
    
    #Update SPC chart
    newGraph <- c(isolate(lineGraph$data), sampleMean)
    lineGraph$data <- newGraph
    
    #Calculate the process capability indices
    cpl <- (gpAve - lcl)/(3 * sampleStDev)
    cul <- (ucl - gpAve)/(3 * sampleStDev)
    cpk <- min(cpl,cul)
    
    #Render valueBoxes
    output$cpkBox <- renderValueBox({
      valueBox(
        round(cpk, digits = 3), "Process Capability", icon = icon("hashtag"),
        if(cpk<1){
          color = "red"
        }
        else
        {
          color="green"
        }
      )
    })
    output$totalAreaPercentageBox <- renderValueBox({
      valueBox(
        paste(round(totalAreaPercentage, digits = 3),"%",sep = " "), "Defect Probability", icon = icon("percent"),
        if(cpk<1){
          color = "red"
        }
        else
        {
          color="green"
        }
      )
    })
    
    
    #Tell the user if defective material is being made or not
    if(cpk<1){
      displayMessage <- paste("Your process capability is",round(cpk, digits = 3),"so defective materials are being made",sep = " ")
      output$bad <- renderText({displayMessage})
      output$good <- renderText({""})
    }
    else{
      displayMessage <- paste("Your process capability value is",round(cpk, digits = 3),"so defective materials are not being made",sep = " ")
      output$good <- renderText({displayMessage})
      output$bad <- renderText({""})
    }
    
    #Calculate the defective material percentage
    upperArea <- pnorm(ucl, mean = sampleMean, sampleStDev, lower.tail = FALSE, log.p = FALSE)
    lowerArea <- pnorm(lcl, mean = sampleMean, sampleStDev, lower.tail = TRUE, log.p = FALSE)
    totalAreaPercentage <- (upperArea + lowerArea) * 100
    
    defPercentageMessage <- paste("The probability of obtaining a defective item:", round(totalAreaPercentage, digits = 3),"%",sep = " ")
    output$defPercentage <- renderText({defPercentageMessage})
    
    contentsMessage <- ""
    
    
    #use p norm
    #for ucl, lower tail false
    #for lcl, lower tail true
    
  })
}

shinyApp(ui = ui, server = server)
