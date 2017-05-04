library(shiny)

shinyUI(fluidPage(
  # Application title
  titlePanel("Activity Test"),
  mainPanel(
    fileInput("rdmlFileInput",
              HTML("Upload <b>.RDML</b> file:")),
    uiOutput("targetSlctUI"),
    htmlOutput("activityModelSummary"),
    # tags$ul(
    #   htmlOutput("activityModelSummary", container = tags$li, class = "custom-li-output")
    # ),
    plotOutput("calibrationPlot"),
    tableOutput("resultsTable")
  )
))
