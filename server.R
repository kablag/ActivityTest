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

  output$quantitiesSlctUI <- renderUI({
    req(description())
    quantities <- unique(description()$quantity)
    quantities <- quantities[!is.na(quantities)]
    selectInput("quantitiesSlct",
                "Use Input Units",
                quantities,
                quantities,
                multiple = TRUE)
  })

  fData <- reactive({
    req(description())
    rdmlFile()$GetFData(description())
  })

  mlist <- reactive({
    req(fData())
    ml1 <- modlist(fData(), model = l5, verbose = FALSE)
    names(ml1) <- colnames(fData())[-1]
    ml1
  })

  slopeResultsTbl <- reactive({
    req(mlist())
    isolate({
      effs <- map(mlist(), ~
                    tryCatch(
                      efficiency(., plot = FALSE),
                      error = function(e) list(cpD1 = NA,
                                               cpD2 = NA)
                    )
      )
      description() %>%
        group_by(fdata.name) %>%
        mutate(cpD1 = effs[[fdata.name]][["cpD1"]],
               cpD2 = effs[[fdata.name]][["cpD2"]],
               cpD1fluo = tryCatch(
                 predict(mlist()[[fdata.name]],
                         newdata = data.frame(Cycles = cpD1))[1,1],
                 error = function(e) NA),
               cpD2fluo = tryCatch(
                 predict(mlist()[[fdata.name]],
                         newdata = data.frame(Cycles = cpD2))[1,1],
                 error = function(e) NA),
               slope = round((cpD1fluo - cpD2fluo)/(cpD1 - cpD2), 2))
    })
  })

  activityModel <- reactive({
    req(slopeResultsTbl(), input$quantitiesSlct)
    stdTbl <- slopeResultsTbl() %>%
      filter(sample.type == "std",
             quantity %in% input$quantitiesSlct)
    Slope <- stdTbl$slope
    Quantity <- stdTbl$quantity
    # lm(log(Quantity) ~ Slope)
    lm(Quantity ~ Slope)
  })

  output$activityModelSummary <- renderUI({
    req(activityModel())
    HTML(sprintf("<div>R<sup>2</sup> = %.4f</div>",
                 summary(activityModel())$r.squared))
  })

  punitsResultsTbl <- reactive({
    req(activityModel())
    isolate({
      slopeResultsTbl() %>%
        mutate(
          # punits = exp(predict(activityModel(), newdata = data.frame(Slope = slope)))
          prediction = list(predict(activityModel(), newdata = data.frame(Slope = slope),
                                    interval = c("prediction"))),
          punits = unlist(prediction)[1] %>% round(2),
          punitsSE = unlist(prediction)[2] %>% round(2)) %>%
        group_by(sample) %>%
        mutate(
          meanPunits = mean(punits) %>%
            round(2),
          meanPunitsSE = mean(punitsSE) %>%
            round(2)) %>%
        ungroup()
    })
  })

  output$resultsTable <- DT::renderDataTable({
    req(punitsResultsTbl())
    punitsResultsTbl() %>%
      mutate(stockUnits = meanPunits * input$stockFactor) %>%
      select(Well = position,
             Name = sample,
             Type = sample.type,
             "Input Units" = quantity,
             "RFU/Cycle" = slope,
             "Pred. Units" = punits,
             "Pred. Units Mean" = meanPunits,
             "Pred. Units Mean SE" = meanPunitsSE,
             "Stock Units" = stockUnits
             ) %>%
      DT::datatable(options = list(paging = FALSE),
                    selection = "single")
  })

  output$curvePlot <- renderPlot({
    req(input$resultsTable_rows_selected)
    selected <- punitsResultsTbl() %>%
      filter(position ==
               as.character(punitsResultsTbl()
                                      [input$resultsTable_rows_selected, "position"])) %>%
      as.data.frame()
    curveName <- selected[1, "fdata.name"]
    prediction <- predict(mlist()[[curveName]])$Prediction
    plot(x = 1:length(prediction),
         y = prediction,
         type = "l",
         xlab = "Cycle", ylab = "RFU", col = "grey", lwd = 2)
    lines(x = c(selected[1, "cpD2"],
            selected[1, "cpD1"]),
          y = c(selected[1, "cpD2fluo"],
            selected[1, "cpD1fluo"]),
          col = "red", lwd = 2)
    points(x = fData()$cyc, y = fData()[[curveName]])
    # (c(cpD2, cpD1), c(cpD2fluo, cpD1fluo), col = "red", size = 5)
  })

  output$calibrationPlot <- renderPlot({
    req(punitsResultsTbl())
    stdTbl <- punitsResultsTbl() %>%
      filter(sample.type == "std")
    unknTbl <- punitsResultsTbl() %>%
      filter(sample.type == "unkn")
    plot(stdTbl$quantity, stdTbl$slope,
         xlab = "Units", ylab = "RFU/Cycle")
    # lines(exp(predict(activityModel())), stdTbl$slope, lty = 3, col = "red")
    lines(stdTbl$punits, stdTbl$slope,
          lty = 3, col = "red", lwd = 2)
    points(unknTbl$punits, unknTbl$slope,
           col = "blue")
    # ggplot() +
    #   geom_point(data = unknTbl, aes(x=quantity, y=slope), color = "grey") +
    #   geom_point(data = unknTbl, aes(x=punits, y=slope, color = sample)) +
    #   theme_bw()
  })


})
