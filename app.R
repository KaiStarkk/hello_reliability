#Load required libraries
library(shiny)
library(shinydashboard)
library(shinyjs)
library(Matrix)
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
                div(htmlOutput('errorLine'),style="color:red"),
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
              textOutput("contents"),
              fluidRow(
                valueBoxOutput("cpkBox", width = 6),
                valueBoxOutput("totalAreaPercentageBox", width = 6)
              ),
              div(textOutput('good'),style="color:green"),
              div(textOutput('bad'),style="color:red"),
              textOutput('firstCheckDisplay'),
              textOutput('secondCheckDisplay'),
              textOutput('thirdCheckDisplay'),
              textOutput('fourthCheckDisplay'),
              textOutput('fifthCheckDisplay'),
              textOutput('sixthCheckDisplay'),
              textOutput('seventhCheckDisplay')
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
            h3("Clear Grand Process"),
            p("This will delete all sample and group data. Please back up all required information before proceeding."),
            actionButton("resetGrandProcessActionButton", "Clear", class="btn-danger")
          )
        ),
        fluidRow(
          box(
            width = 12, height = 220, status = "primary", solidHeader = TRUE, title = "Reset Base Class",
            h3("Upload Good Class"),
            p("This will upload a file that is then used to reset the base class against which all future samples are checked."),
            textOutput('fileHelperHolder'),
            fileInput('file1', 'Choose file to upload',
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
  
  added <- NULL #String containing the type of data most recently added to the grand process
  loaded <- TRUE #Boolean value, determines whether or not the baseline process parameters and data are loaded
  
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
  
  #Recalculate our 'globals' in the event a new file was uploaded
  output$fileHelperHolder <- renderText({
    inFile <- input$file1
    if (is.null(inFile)){
      return(NULL)
    }
    #There is a file? Recalculate then
    baseLineFile <- read.csv(inFile$datapath, header = input$header,
             sep = input$sep, quote = input$quote)
    baseLineData <- baseLineFile[,2]
    
    gpAve <- mean(baseLineData[,2])
    maxMinList <- range(baseLineData[,2])
    rangeAve <- maxMinList[2]-maxMinList[1]
    
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
    
    lineGraph$data <- ""
    placeHolder <- ""
  })
  
  
  
  #Render the SPC Chart
  output$SPCChart <- renderPlot({
    if (!is.null(lineGraph$data)) {
      
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
    }
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
  
  #"Clear Grand Process" button
  observeEvent(input$resetGrandProcessActionButton, {
    lineGraph$data <- NULL
    loaded <<- FALSE
  })
  
  #"Upload Good Class" button
  observeEvent(input$uploadGoodClassActionButton, {
    lineGraph$data <- NULL
    loaded <<- FALSE
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
    
    
    numberOfTriggers <- 0
    output$cpkBox <- renderValueBox({
      valueBox(
        numberOfTriggers, "Triggered Checks", icon = icon("hashtag"), #Round used to take cpk as
        if(numberOfTriggers){
          color = "red"
        }
        else
        {
          color="green"
        }
      )
    })
    
    #Check that a grand process is actually loaded
    if (!loaded) {
      output$errorLine <- renderUI({HTML(paste("No process is currently loaded.", "Navigate to the settings page to upload the baseline data.","","", sep = '<br/>'))})
      output$bad <- renderText({""})
      output$defPercentage <- renderText({""})
      output$good <- renderText({""})
      checkResults <- vector(mode="integer", length=7)
      output$firstCheckDisplay <- renderText({""})
      output$secondCheckDisplay <- renderText({""})
      output$thirdCheckDisplay <- renderText({""})
      output$fourthCheckDisplay <- renderText({""})
      output$fifthCheckDisplay <- renderText({""})
      output$sixthCheckDisplay <- renderText({""})
      output$seventhCheckDisplay <- renderText({""})
      return(NULL)
    }
    
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
    
    #Clear previous errors
    output$errorLine <- renderText({""})
    
    #Calculate the stats associated with the new sample
    sampleMean <- mean(newSample)
    
    #Update SPC chart
    newGraph <- c(isolate(lineGraph$data), sampleMean)
    lineGraph$data <- newGraph
    
    #Run the checks
    checkResults <- vector(mode="integer", length=7)
    checkResults[1] <- firstCheck(gpAve = gpAve, sampleMeans = lineGraph$data, zoneDf = zoneDf)
    checkResults[2] <- secondCheck(gpAve = gpAve, sampleMeans = lineGraph$data, zoneDf = zoneDf)
    checkResults[3] <- thirdCheck(gpAve = gpAve, sampleMeans = lineGraph$data, zoneDf = zoneDf)
    checkResults[4] <- fourthCheck(gpAve = gpAve, sampleMeans = lineGraph$data, zoneDf = zoneDf)
    checkResults[5] <- fifthCheck(gpAve = gpAve, sampleMeans = lineGraph$data, zoneDf = zoneDf)
    checkResults[6] <- sixthCheck(gpAve = gpAve, sampleMeans = lineGraph$data, zoneDf = zoneDf)
    checkResults[7] <- seventhCheck(gpAve = gpAve, sampleMeans = lineGraph$data, zoneDf = zoneDf)
    
    numberOfTriggers <- nnzero(checkResults)
    
    #Render valueBoxes
    #This is the 'Process Capability' box
    output$cpkBox <- renderValueBox({
      valueBox(
        numberOfTriggers, "Triggered Checks", icon = icon("hashtag"), #Round used to take cpk as
        if(numberOfTriggers){
          color = "red"
        }
        else
        {
          color="green"
        }
      )
    })
    #This is the 'Defect Probability' box
    #output$totalAreaPercentageBox <- renderValueBox({
    # valueBox(
    #  "","Check Details", icon = icon("percent"),
    # if(numberOfTriggers){
    #  color = "red"
    #}
    #else
    #{
    #color="green"
    #}
    #)
    #})
    
    
    #Tell the user if the process is out of control or not
    if(numberOfTriggers==0){ #Fake for now
      displayMessage <- "Your process is in control"
      output$bad <- renderText({""})
      output$good <- renderText({displayMessage})
      output$defPercentage <- renderText({""})
    }
    else{
      displayMessage <- "Your process is out of control"
      output$bad <- renderText({displayMessage})
      output$good <- renderText({""})
      
      #Provide details
      checkDetailsHeader <- "Here are the details"
      
      checkDetails <-""
      
      if(checkResults[1]>0){
        output$firstCheckDisplay <- renderText({paste("Point",checkResults[1],"is outside your control limits")})
      }
      else{
        output$firstCheckDisplay <- renderText({""})
      }
      if(checkResults[2]>0){
        output$secondCheckDisplay <- renderText({paste("Point",checkResults[2],"marks the start of 3 consecutive points within which 2 are in Zone A")})
      }
      else{
        output$secondCheckDisplay <- renderText({""})
      }
      if(checkResults[3]>0){
        output$thirdCheckDisplay <- renderText({paste("Point",checkResults[3],"marks the start of 5 consecutive points within which 4 are in Zone A or B")})
      }
      else{
        output$thirdCheckDisplay <- renderText({""})
      }
      if(checkResults[4]>0){
        output$fourthCheckDisplay <- renderText({paste("Point",checkResults[4],"marks the start of 9 consecutive points that lie on a one side of the average")})
      }
      else{
        output$fourthCheckDisplay <- renderText({""})
      }
      if(checkResults[5]>0){
        output$fifthCheckDisplay <- renderText({paste("Point",checkResults[5],"marks the start of 6 consecutive points that are monotonically increasing or decreasing")})
      }
      else{
        output$fifthCheckDisplay <- renderText({""})
      }
      if(checkResults[6]>0){
        output$sixthCheckDisplay <- renderText({paste("Point",checkResults[6],"marks the start of 14 consecutive points that oscillate")})
      }
      else{
        output$sixthCheckDisplay <- renderText({""})
      }
      if(checkResults[7]>0){
        output$seventhCheckDisplay <- renderText({paste("Point",checkResults[7],"marks the start of 15 consecutive points that lie in Zone C")})
      }
      else{
        output$seventhCheckDisplay <- renderText({""})
      }
      
      output$defPercentage <- renderText({capture.output(cat(checkDetails))})
    }
    
    
    
    contentsMessage <- ""
    
    
  })
}

shinyApp(ui = ui, server = server)
