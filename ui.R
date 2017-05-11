library(shiny)

shinyUI(fluidPage(
  # Application title
  titlePanel("Activity Test"),
  mainPanel(
    fileInput("rdmlFileInput",
              HTML("Upload <b>.RDML</b> file:")),
    fluidRow(
      column(4,
             uiOutput("targetSlctUI")),
      column(4,
             uiOutput("quantitiesSlctUI")),
      column(4,
             numericInput("stockFactor", "Stock Factor", 25, 1, step = 1))),
    # tags$ul(
    #   htmlOutput("activityModelSummary", container = tags$li, class = "custom-li-output")
    # ),
    fluidRow(
      column(6,
             plotOutput("calibrationPlot")),
      column(6,
             plotOutput("curvePlot"))),
    htmlOutput("activityModelSummary"),
    DT::dataTableOutput("resultsTable")
  )
))
