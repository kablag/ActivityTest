library(shiny)

shinyUI(fluidPage(
  # Application title
  titlePanel("Activity Test"),
  mainPanel(
    fileInput("rdmlFileInput",
              HTML("Upload <b>.RDML</b> file:")),
    uiOutput("targetSlctUI")
  )
))
