library(shiny)
library(plotly)

shinyUI(fluidPage(
  # Application title
  titlePanel("Activity Test"),
  mainPanel(
    fileInput("rdmlFileInput",
              HTML("Upload <b>.RDML</b> file:")),
    fluidRow(
      column(3,
             checkboxInput("addNPoints", "Add N Points", FALSE)),
      column(3,
             selectInput("modelName",
                         "Curve Model",
                         c("l7", "l6", "l5", "l4", "b7", "b6", "b5",
                           "b4", "expGrowth", "expSDM", "linexp",
                           "mak2", "mak2i", "mak3", "mak3i",
                           "lin2", "cm3", "spl3"),
                         "l5")),
      column(3,
             htmlOutput("selectedModelDescription")),
      column(3,
             radioButtons("calibrationParameter",
                         "Calibration Parameter",
                         c("slope", "e"),
                         "slope"))
      ),
    fluidRow(
      column(4,
             uiOutput("targetSlctUI")),
      column(4,
             numericInput("dilutionFactor", "Dilution Factor", 5, 1, step = 1)),
      column(4,
             numericInput("stockFactor", "Stock Factor", 25, 1, step = 1))
    ),
    uiOutput("quantitiesSlctUI"),
    # tags$ul(
    #   htmlOutput("activityModelSummary", container = tags$li, class = "custom-li-output")
    # ),
    plotlyOutput("calibrationPlot"),
    fluidRow(
      column(6,
             plotlyOutput("meltPlot")),
      column(6,
             plotlyOutput("curvePlot"))
      ),
    htmlOutput("activityModelSummary"),
    DT::dataTableOutput("resultsTable")
  )
))
