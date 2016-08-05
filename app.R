library(shiny)
library(shinydashboard)

## ------------------------------
## UI functionality
## ------------------------------

ui <- dashboardPage(
  dashboardHeader(title = "Reliability Application"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Grand Process Analysis", tabName = "grand-process-analysis", icon = icon("dashboard"), badgeLabel = "beta", badgeColor = "purple"),
      menuItem("Settings", tabName = "settings", icon = icon("gears"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      tabItem(
        tabName = "grand-process-analysis",
        h2("Grand Process Analysis"),
        fluidRow(
          column(
            width = 8,
            fluidRow(
              width = NULL,
                  infoBox("Grand Process Parameters", p(span(HTML("X&#x33F;"), "= 50"), br(),
                                                        span(HTML("R&#x305;"), "= 100")
                                                        ), icon = icon("info-circle")),
                  infoBox("Fitted Distribution Type", "Normal", icon = icon("area-chart")),
                  infoBox("Fitted Distribution Parameters", p(span(HTML("&mu;"), "= 50"), br(), span(HTML("&sigma;"), "= 100")), icon = icon("info"))
            ),
            tabBox(
              width = NULL,
              id = "grand-process-tabBox",
              tabPanel(
                "Control Chart",
                plotOutput("controlChart",
                           click = "plot_click",
                           dblclick = "plot_dblclick",
                           hover = "plot_hover",
                           brush = "plot_brush")
                ),
              tabPanel(
                "Table View",
                dataTableOutput('tableView')
              )
            )
          ),
          column(
            width = 4,
            box(
              width = NULL,
              title = "Upload Samples",
              "Select a group or group-set in csv format.",
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
            ),
            box(
              width = NULL,
              title = "Results",
              p(span(HTML("&alpha;"))," Value: 0.05"),
              textOutput('contents'),
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
            width = 6,
            column(
              width = 8,
              textInput("usl-textInput", "Set USL", value = "70")
            ),
            column(
              width = 4,
              actionButton("usl-actionButton", "Set")
            )
          )
        ),
        fluidRow(
          box(
            width = 6,
            column(
              width = 8,
              textInput("lsl-textInput", "Set LSL", value = "30")
            ),
            column(
              width = 4,
              actionButton("lsl-actionButton", "Set")
            )
          )
        ),
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

  #Set the initial characteristics of the GP
  popMean <- 50
  popRangeMean <- 10
  popStdev <- 5

  #Set the upper and lower limits
  lsl <- 30
  usl <- 70

  # By default, the file size limit is 5MB. It can be changed by
  # setting this option. Here we'll raise limit to 9MB.
  options(shiny.maxRequestSize = 9*1024^2)

  output$controlChart <- renderPlot({
    peak <- dnorm(popMean, popMean, 5)
    curve(dnorm(x, popMean, 5), lsl - (popMean-lsl)/2, usl + (usl-popMean)/2, col="darkblue", xlab = "", ylab = "", ylim = c (0, peak*1.1))
    abline(v=lsl, lty=2)
    text(lsl, peak, labels="LSL", pos=4)
    abline(v=usl, lty=2)
    text(usl, peak, labels="USL", pos=2)
    arrows(popMean, 0, popMean, peak, 0, lty=4)
    text(popMean, peak, labels="X\U33F", pos=3)
  })

  output$tableView = renderDataTable(
    data.frame(Group=1:100, Mean=rnorm(100, popMean, 5), Range=rnorm(100, popRangeMean, 5)),
    options = list(pageLength = 10)
  )


  output$contents <- renderText({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.

    inFile <- input$file1

    if (is.null(inFile))
      return(NULL)

    #Read in the new sample
    inputData <- read.csv(inFile$datapath)
    newSample <- inputData[,2]

    #Calculate the stats associated with the new sample
    sampleMean <- mean(newSample)
    sampleSize <- length(newSample)
    sampleRange <- diff(range(newSample))
    stDevOptions <- c(NaN,1.128,1.693,2.059,2.326,2.534,2.704,2.847,2.970,3.078)
    sampleStDev <- stDevOptions[sampleSize]

    #Update the population stats
    popMean <- (popMean + sampleMean)/2
    popRangeMean <- (popRangeMean + sampleRange)/2

    #Calculate the process capability indices
    cpl <- (popMean - lsl)/(3 * sampleStDev)
    cul <- (usl - popMean)/(3 * sampleStDev)
    cpk <- min(cpl,cul)

    print("popMean")
    print(popMean)
    print("lsl")
    print(lsl)
    print("usl")
    print(usl)

    print("sampleMean")
    print(sampleMean)
    print("sampleStDev")
    print(sampleStDev)

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
      displayMessage <- paste("Your process capability is",cpk,"so defective materials are being made",sep = " ")
      output$bad <- renderText({displayMessage})
      output$good <- renderText({""})
    }
    else{
      displayMessage <- paste("Your process capability value is",cpk,"so defective materials are not being made",sep = " ")
      output$good <- renderText({displayMessage})
      output$bad <- renderText({""})
    }

    #Calculate the defective material percentage
    upperArea <- pnorm(usl, mean = sampleMean, sampleStDev, lower.tail = FALSE, log.p = FALSE)
    lowerArea <- pnorm(lsl, mean = sampleMean, sampleStDev, lower.tail = TRUE, log.p = FALSE)
    totalAreaPercentage <- (upperArea + lowerArea) * 100

    defPercentageMessage <- paste("The probability of obtaining a defective item:", totalAreaPercentage,"%",sep = " ")
    output$defPercentage <- renderText({defPercentageMessage})

    contentsMessage <- ""


    #use p norm
    #for usl, lower tail false
    #for lsl, lower tail true

  })
}

shinyApp(ui = ui, server = server)
