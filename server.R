library(RDML)
library(qpcR)
library(tidyverse)
library(shiny)

shinyServer(function(input, output, session) {
  session$onSessionEnded(function() {
    stopApp()
  })

  rdmlFile <- reactive({
    req(input$rdmlFileInput)
    RDML$new(input$rdmlFileInput$datapath)
  })

  description <- reactive({
    req(rdmlFile())
    rdmlFile()$AsTable(quantity = sample[[react$sample$id]]$quantity$value)
  })

  output$targetSlctUI <- renderUI({
    req(rdmlFile())
    selectInput("targetSlct",
                "Target",
                names(rdmlFile()$target))
  })

  stdDescription <- reactive({
    req(input$targetSlct, description())
    description() %>% filter(target == input$targetSlct, sample.type == "std")
  })

  stdDescription <- reactive({
    req(input$targetSlct, description())
    description() %>% filter(target == input$targetSlct, sample.type == "std")
  })

})
