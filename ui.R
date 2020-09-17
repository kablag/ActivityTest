library(shiny)
library(plotly)

shinyUI(fluidPage(
  # Application title
  titlePanel("Activity Test"),
  sidebarLayout(
    # tabsetPanel(

    # Main Panel --------------------------------------------------------------


    mainPanel("Main",
              fileInput("rdmlFileInput",
                        HTML("Upload <b>.RDML</b> file:")),
              fluidRow(
                # column(3,
                #        checkboxInput("addNPoints", "Add N Points", FALSE)),
                # column(3,
                #        selectInput("modelName",
                #                    "Curve Model",
                #                    c("l7", "l6", "l5", "l4", "b7", "b6", "b5",
                #                      "b4", "expGrowth", "expSDM", "linexp",
                #                      "mak2", "mak2i", "mak3", "mak3i",
                #                      "lin2", "cm3", "spl3"),
                #                    "l5")),
                # column(3,
                #        htmlOutput("selectedModelDescription")),
                column(3,
                       radioButtons("calibrationParameter",
                                    "Calibration Parameter",
                                    c("slope",
                                      # "e",
                                      "end point"),
                                    "slope")),
                column(4,
                       checkboxInput("useGlobalSlope",
                                     "Use Global Slope", value = TRUE),
                       sliderInput("slopeRegion",
                                   "Slope Region", min = 1, max = 50, c(10, 20),
                                   step = 1)
                )),
              fluidRow(
                column(3,
                       uiOutput("targetSlctUI")),
                column(3,
                       numericInput("dilutionFactor", "Dilution Factor", 5, 1, step = 1)),
                column(3,
                       numericInput("stockFactor", "Stock Factor", 25, 1, step = 1)),
                column(3,
                       numericInput("endPointCycle", "End Point Cycle", 20, 1, step = 1))#,
                # column(2,
                #        checkboxInput("autoEndPointCycle", "Auto End Point Cycle", TRUE))
              ),
              uiOutput("quantitiesSlctUI"),
              # tags$ul(
              #   htmlOutput("activityModelSummary", container = tags$li, class = "custom-li-output")
              # ),
              uiOutput("useWellsUI"),
              actionButton("recalcBtn", "Recalc"),
              uiOutput("unknSmplUI"),
              # actionButton("applySlopes", "Apply"),
              plotlyOutput("calibrationPlot"),
              htmlOutput("activityModelSummary"),
              fluidRow(
                column(6,
                       plotlyOutput("meltPlot")),
                column(6,
                       plotlyOutput("curvePlot"))
              ),
              DT::dataTableOutput("resultsTable")
    ),

    # Slope Regions -----------------------------------------------------------

    sidebarPanel(h5("Slope Regions"),
                 uiOutput("individualSlopeRegionsUI"))
  )
))

