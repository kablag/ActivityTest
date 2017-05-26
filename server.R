library(RDML)
library(qpcR)
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(stringr)
library(plotly)

ADD_N_POINTS <- 20

shinyServer(function(input, output, session) {
  session$onSessionEnded(function() {
    stopApp()
  })

  addNPoints <- reactive({
    if (input$addNPoints)
      ADD_N_POINTS
    else
      0
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
      filter(target == input$targetSlct,
             adp == TRUE) %>%
      mutate(logQuantity = log(quantity))
  })

  output$quantitiesSlctUI <- renderUI({
    req(description())
    quantities <- unique(description()$quantity)
    quantities <- sort(quantities[!is.na(quantities)])
    # selectInput("quantitiesSlct",
    #             "Use Input Units",
    #             quantities,
    #             quantities,
    #             multiple = TRUE)
    checkboxGroupButtons(
      inputId = "quantitiesSlct", label = "Use Input Units :",
      choices = quantities,
      selected = quantities,
      justified = TRUE, status = "default",
      #'arg' should be one of “default”, “primary”, “success”, “info”, “warning”, “danger”
      checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
    )
  })

  fData <- reactive({
    req(description())
    rdmlFile()$GetFData(description())
  })

  output$selectedModelDescription <- renderUI({
    req(input$modelName)
    HTML(
      paste(eval(parse(text = input$modelName))$name,
            eval(parse(text = input$modelName))$type,
            eval(parse(text = input$modelName))$expr,
            sep = "</br>")
    )
  })

  mlist <- reactive({
    req(fData())
    # ml1 <- modlist(fData(), model = eval(parse(text = input$modelName)),
    #                verbose = FALSE)

    ml2 <- map(colnames(fData())[-1],
               ~ {
                 fpoints <- data.frame(cyc = 1:(addNPoints() + nrow(fData())),
                                       fluor = c(rep(first(fData()[[.]]), addNPoints()),
                                                 fData()[[.]]))
                 tryCatch(
                   pcrfit(
                     fpoints,
                     fluo = 2,
                     model = eval(parse(text = input$modelName))
                   ),
                   error = function(e) NA
                 )
               }
    )
    names(ml2) <- colnames(fData())[-1]
    ml2
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
        mutate(
          cpD1 = effs[[fdata.name]][["cpD1"]],
          cpD2 = effs[[fdata.name]][["cpD2"]],
          cpD1fluo = tryCatch(
            predict(mlist()[[fdata.name]],
                    newdata = data.frame(Cycles = cpD1))[1,1],
            error = function(e) NA),
          cpD2fluo = tryCatch(
            predict(mlist()[[fdata.name]],
                    newdata = data.frame(Cycles = cpD2))[1,1],
            error = function(e) NA),
          slope = round((cpD1fluo - cpD2fluo)/(cpD1 - cpD2), 2)
          # coefE = mlist()[[fdata.name]]$model$e
        )
    })
  })

  activityModel <- reactive({
    req(slopeResultsTbl(), input$quantitiesSlct)
    stdTbl <- slopeResultsTbl() %>%
      filter(sample.type == "std",
             quantity %in% as.numeric(input$quantitiesSlct))
    Slope <- stdTbl$slope
    Quantity <- stdTbl$quantity
    # LogQuantity <- stdTbl$logQuantity
    # CoefE <- stdTbl$coefE
    # lm(log(Quantity) ~ Slope)
    lm(Quantity ~ Slope)
    # lm(LogQuantity ~ CoefE)
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
                                    interval = c("confidence"))),
          # prediction = list(exp(predict(activityModel(), newdata = data.frame(CoefE = coefE),
          #                           interval = c("confidence")))),
          punits = unlist(prediction)[1] %>% round(2),
          punitsPI = (unlist(prediction)[1] - unlist(prediction)[2]) %>%
            round(2)) %>%
        group_by(sample) %>%
        mutate(
          meanPunits = mean(punits) %>%
            round(2),
          meanPunitsPI = mean(punitsPI) %>%
            round(2)) %>%
        ungroup()
    })
  })

  output$resultsTable <- DT::renderDataTable({
    req(punitsResultsTbl())
    calcStock <- function(dilutionN, meanPunits, stockFactor, dilutionFactor) {
      if (is.na(dilutionN))
        dilutionN <- 1
      meanPunits * stockFactor * dilutionFactor ^ (dilutionN - 1)
    }
    punitsResultsTbl() %>%
      mutate(
        dilutionN = str_extract(sample, "[0-9]+") %>%
          as.integer(),
        stockUnits = calcStock(dilutionN, meanPunits,
                               input$stockFactor, input$dilutionFactor),
        stockUnitsPI = calcStock(dilutionN, meanPunitsPI,
                                 input$stockFactor, input$dilutionFactor),
        punitsText = sprintf("%.2f ± %.2f", meanPunits, meanPunitsPI),
        sunitsText = sprintf("%.2f ± %.2f", stockUnits, stockUnitsPI)) %>%
      select(Well = position,
             Name = sample,
             Type = sample.type,
             "Input Units" = quantity,
             "RFU/Cycle" = slope,
             # "E" = coefE %>% round(2),
             "Pred. Units" = punits,
             "Pred. Units Mean" = punitsText,
             # "Pred. Units Mean" = meanPunits,
             # "Pred. Units ± Mean (PI)" = meanPunitsPI,
             "Dilution N" = dilutionN,
             # "Stock Units" = stockUnits,
             # "Stock Units ± (PI)" = stockUnitsPI
             "Stock Units" = sunitsText
      ) %>%
      DT::datatable(
        data = .,
        options = list(paging = FALSE),
        selection = "single")
  })

  output$curvePlot <- renderPlotly({
    req(input$resultsTable_rows_selected)
    selected <- punitsResultsTbl() %>%
      filter(position ==
               as.character(punitsResultsTbl()
                            [input$resultsTable_rows_selected, "position"])) %>%
      as.data.frame()
    curveName <- selected[1, "fdata.name"]
    fdt <- data.frame(cyc = fData()[["cyc"]],
                      fluor = fData()[[curveName]],
                      prediction = {
                        if (!is.na(selected[1, "punits"])) {
                          predict(mlist()[[curveName]])$Prediction %>%
                          {
                            if (addNPoints())
                              .[-c(1:addNPoints())]
                            else
                              .
                          }
                        } else {
                          NaN
                        }
                      })
    slopeLine <- data.frame(cyc = c(selected[1, "cpD2"] - addNPoints(),
                                    selected[1, "cpD1"] - addNPoints()),
                            fluor = c(selected[1, "cpD2fluo"],
                                      selected[1, "cpD1fluo"]))
    ggplot(fdt) +
      geom_point(aes(cyc, fluor)) +
      geom_line(aes(x = cyc, y = prediction), color = "grey") +
      geom_line(data = slopeLine, aes(x = cyc, y = fluor), color = "red") +
      theme_bw()
  })

  output$meltPlot <- renderPlotly({
    req(input$resultsTable_rows_selected)
    selected <- punitsResultsTbl() %>%
      filter(position ==
               as.character(punitsResultsTbl()
                            [input$resultsTable_rows_selected, "position"])) %>%
      as.data.frame()
    # curveName <- selected[1, "fdata.name"]
    meltData <- tryCatch(
      rdmlFile()$GetFData(selected, dp.type = "mdp", long.table = TRUE),
      error = function(e) NULL)
    req(meltData)
    meltData$diffFluor <- diff(meltData$fluor) * -1
    ggplot(meltData, aes(tmp, diffFluor)) +
      geom_line() +
      theme_bw()
  })

  output$calibrationPlot <- renderPlotly({
    req(punitsResultsTbl())
    # input$quantitiesSlct
    stdTbl <- punitsResultsTbl() %>%
      filter(sample.type == "std")
    unknTbl <- punitsResultsTbl() %>%
      filter(sample.type == "unkn")
    selectedQuantities <- as.numeric(input$quantitiesSlct)
    ggplot(stdTbl %>%
             filter(quantity %in% selectedQuantities),
           aes(x = quantity, y = slope)) +
      geom_smooth(method = "lm", color = "red", fill = "red") +
      geom_point(size = 3, aes(group = sample)) +
      geom_point(data = stdTbl %>%
                   filter(!(quantity %in% selectedQuantities)),
                 color = "grey60", size = 3, aes(group = sample)) +
      geom_point(data = unknTbl %>%
                   filter(punits <= max(selectedQuantities) &
                            punits >= min(selectedQuantities)),
                 aes(x = punits, y = slope, group = sample),
                 shape = 24, fill = "green4", size = 3) +
      geom_point(data = unknTbl %>%
                   filter(punits > max(selectedQuantities) |
                            punits < min(selectedQuantities)),
                 aes(x = punits, y = slope, group = sample),
                 shape = 24, fill = "orange", size = 3) +
      theme_bw()
  })



})
