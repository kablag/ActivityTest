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

  output$targetSlctUI <- renderUI({
    req(rdmlFile())
    selectInput("targetSlct",
                "Target",
                names(rdmlFile()$target))
  })

  description <- reactive({
    req(rdmlFile(), input$targetSlct)
    rdmlFile()$AsTable(quantity = sample[[react$sample$id]]$quantity$value) %>%
      filter(target == input$targetSlct)
  })

  fData <- reactive({
    req(description())
    rdmlFile()$GetFData(description())
  })

  mlist <- reactive({
    req(fData())
    ml1 <- modlist(fData(), model = l5)
    names(ml1) <- colnames(fData())[-1]
    ml1
  })

  slopeResultsTbl <- reactive({
    req(mlist())
    isolate({
      effs <- map(mlist(), ~ efficiency(., plot = FALSE))
      description() %>%
        group_by(fdata.name) %>%
        mutate(cpD1 = effs[[fdata.name]][["cpD1"]],
               cpD2 = effs[[fdata.name]][["cpD2"]],
               cpD1fluo = predict(mlist()[[fdata.name]],
                                  newdata = data.frame(Cycles = cpD1))[1,1],
               cpD2fluo = predict(mlist()[[fdata.name]],
                                  newdata = data.frame(Cycles = cpD2))[1,1],
               slope = (cpD1fluo - cpD2fluo)/(cpD1 - cpD2))
    })
  })

  activityModel <- reactive({
    req(slopeResultsTbl())
    stdTbl <- slopeResultsTbl() %>%
      filter(sample.type == "std")
    Slope <- stdTbl$slope
    Quantity <- stdTbl$quantity
    # lm(log(Quantity) ~ Slope)
    lm(Quantity ~ Slope)
  })

  output$activityModelSummary <- renderText({
    req(activityModel())
    # aa <<- activityModel()
    # print(summary(activityModel()))
    activityModel()$r.squared
  })

  punitsResultsTbl <- reactive({
    req(activityModel())
    srt <<- slopeResultsTbl()
    acm <<- activityModel()
    isolate({
      slopeResultsTbl() %>%
        ungroup() %>%
        mutate(
          # punits = exp(predict(activityModel(), newdata = data.frame(Slope = slope)))
          punits = predict(activityModel(), newdata = data.frame(Slope = slope))) %>%
        group_by(sample) %>%
        mutate(
          meanPunits = mean(punits)
          ) %>%
        ungroup()
    })
  })

  output$resultsTable <- renderTable({
    req(punitsResultsTbl())
    punitsResultsTbl() %>%

      select(Well = position,
             Name = sample,
             Type = sample.type,
             "Input Units" = quantity,
             "RFU/Cycle" = slope,
             "Predicted Units" = punits,
             "Predicted Units Mean" = meanPunits
             )
  })

  output$calibrationPlot <- renderPlot({
    req(punitsResultsTbl())
    stdTbl <- slopeResultsTbl() %>%
      filter(sample.type == "std")
    unknTbl <- punitsResultsTbl() %>%
      filter(sample.type == "unkn")
    plot(stdTbl$quantity, stdTbl$slope, xlab = "Units", ylab = "RFU/Cycle")
    # lines(exp(predict(activityModel())), stdTbl$slope, lty = 3, col = "red", size = 5)
    lines(predict(activityModel()), stdTbl$slope, lty = 3, col = "red", size = 5)
    points(unknTbl$punits, unknTbl$slope, col = "blue", size = 5)
  })


})
