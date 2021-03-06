library(tools)
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
    cat("loading file\n")
    newpath <- paste(input$rdmlFileInput$datapath[1],
                     file_ext(input$rdmlFileInput$name[1]),
                     sep = ".")
    file.rename(input$rdmlFileInput$datapath[1],
                newpath)
    RDML$new(newpath)
  })

  output$targetSlctUI <- renderUI({
    req(rdmlFile())
    cat("creating targetSlct\n")
    selectInput("targetSlct",
                "Target",
                names(rdmlFile()$target))
  })

  description <- reactive({
    cat("gen description\n")
    req(input$targetSlct)
    rdmlFile()$AsTable(
      quantity =
        sample[[react$sample$id]]$quantity$value) %>%
      filter(target == input$targetSlct,
             adp == TRUE) %>%
      mutate(logQuantity = log(quantity))
  })

  output$quantitiesSlctUI <- renderUI({
    req(description())
    cat("creating quantitiesSlct\n")
    quantities <- unique(description()$quantity)
    quantities <- sort(quantities[!is.na(quantities)])
    checkboxGroupButtons(
      inputId = "quantitiesSlct", label = "Use Input Units :",
      choices = quantities,
      selected = quantities,
      justified = TRUE, status = "default",
      #'arg' should be one of “default”, “primary”, “success”, “info”, “warning”, “danger”
      checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
    )
  })

  output$useWellsUI <- renderUI({
    req(description())
    cat("creating useWellsSlct\n")
    wells <- unique(description()$position)
    selectInput("useWellsSlct", "Use Wells",
                wells, wells, multiple = TRUE)
  })

  fData <- reactive({
    req(description(), input$useWellsSlct)
    cat("getting fdata\n")
    as.data.frame(rdmlFile()$GetFData(description()[position %in% input$useWellsSlct]))
  })

  output$individualSlopeRegionsUI <- renderUI({
    req(description())
    cat("creating individualSlopeRegionsUI\n")
    lapply(unique(description()$sample), function(sname)
      sliderInput(paste0("slopeRegion_", sname),
                  paste0("Slope Region: ", sname),
                  min = 1, max = 50, c(10, 20),
                  step = 1))
  })

  mlist <- reactive({
    req(input$recalcBtn)
    isolate({
      req(fData())
      cat("calc mlist\n")
      ml <- lapply(colnames(fData())[-1],
                   function(cname) {
                     slopeRegion <- {
                       if (input$useGlobalSlope)
                         input$slopeRegion
                       else {
                         sname <- description()[fdata.name == cname, c(sample)]
                         input[[paste0("slopeRegion_", sname)]]
                       }
                     }

                     fpoints <-
                       data.frame(
                         cyc = slopeRegion[1]:slopeRegion[2],
                         fluor = fData()[
                           slopeRegion[1]:slopeRegion[2],
                           cname])
                     tryCatch(
                       lm(fluor ~ cyc, fpoints),
                       error = function(e) NA
                     )
                   })
      names(ml) <- colnames(fData())[-1]
      ml

    })
  })

  slopeResultsTbl <- reactive({
    req(mlist())
    req(input$endPointCycle)
    cat("calc slopeResultsTbl\n")
    isolate({
      description()[position %in% input$useWellsSlct] %>%
        group_by(fdata.name) %>%
        mutate(
          slope = mlist()[[fdata.name]]$coefficients[2],
          endPointRFU = tryCatch(
            predict(mlist()[[fdata.name]],
                    newdata = data.frame(cyc = input$endPointCycle)),
            error = function(e) NA)
        )
    })
  })

  activityModel <- reactive({
    req(slopeResultsTbl(),
        input$quantitiesSlct)
    cat("calc activityModel\n")
    stdTbl <- slopeResultsTbl() %>%
      filter(sample.type == "std",
             quantity %in% as.numeric(input$quantitiesSlct))
    Slope <- stdTbl$slope
    Quantity <- stdTbl$quantity
    LogQuantity <- stdTbl$logQuantity
    # CoefE <- stdTbl$coefE
    EndPointRFU <- stdTbl$endPointRFU
    # lm(log(Quantity) ~ Slope)
    switch(input$calibrationParameter,
           "slope" =  lm(Quantity ~ Slope),
           # "e" = lm(LogQuantity ~ CoefE),
           "end point" = lm(Quantity ~ EndPointRFU)
    )
  })

  output$activityModelSummary <- renderUI({
    req(activityModel())
    cat("printing activityModelSummary\n")
    HTML(sprintf("<div><b>R<sup>2</sup> = %.4f</b></div>",
                 summary(activityModel())$r.squared))
  })

  punitsResultsTbl <- reactive({
    req(activityModel())
    cat("making punitsResultsTbl\n")
    isolate({
      slopeResultsTbl() %>%
        mutate(
          # punits = exp(predict(activityModel(), newdata = data.frame(Slope = slope)))
          prediction = list(predict(activityModel(),
                                    newdata = {
                                      switch(input$calibrationParameter,
                                             "slope" =  data.frame(Slope = slope),
                                             # "e" = data.frame(CoefE = coefE),
                                             "end point" = data.frame(EndPointRFU = endPointRFU)
                                      )
                                      # if (input$calibrationParameter == "slope")
                                      #   data.frame(Slope = slope)
                                      # else
                                      #   data.frame(CoefE = coefE)
                                    },
                                    interval = c("confidence"))),
          # prediction = list(exp(predict(activityModel(), newdata = ,
          #                           interval = c("confidence")))),
          punits = unlist(prediction)[1] %>%
            {
              switch(input$calibrationParameter,
                     "slope" =  .,
                     # "e" = exp(.),
                     "end point" = .
              )

            } %>%
            round(2),
          punitsPI = (punits - {
            switch(input$calibrationParameter,
                   "slope" =  unlist(prediction)[2],
                   # "e" = exp(unlist(prediction)[2]),
                   "end point" = unlist(prediction)[2]
            )

          }) %>%
            round(2)) %>%
        group_by(sample) %>%
        mutate(
          sampleName = str_extract(sample, "[:alpha:]+"),
          meanPunits = mean(punits) %>%
            round(2),
          meanPunitsPI = mean(punitsPI) %>%
            round(2)) %>%
        ungroup()
    })
  })

  results <- reactive({
    req(punitsResultsTbl())
    cat("calc results\n")
    calcStock <- function(dilutionN, meanPunits, stockFactor, dilutionFactor) {
      dilutionN[is.na(dilutionN)] <- 1
      meanPunits * stockFactor * dilutionFactor ^ (dilutionN - 1)
      # meanPunits * stockFactor * dilutionFactor * dilutionN
    }
    punitsResultsTbl() %>%
      mutate(
        dilutionN = str_extract(sample, "[0-9]+") %>%
          as.integer(),
        dilution = input$dilutionFactor ^ (dilutionN - 1),
        stockFactor = ifelse(sample.type == "std",
                             1,
                             input$stockFactor),
        stockUnits = calcStock(dilutionN, meanPunits,
                               stockFactor, input$dilutionFactor),
        stockUnitsPI = calcStock(dilutionN, meanPunitsPI,
                                 stockFactor, input$dilutionFactor),
        punitsText = sprintf("%.2f ± %.2f", meanPunits, meanPunitsPI),
        sunitsText = sprintf("%.2f ± %.2f", stockUnits, stockUnitsPI)) %>%
      select(Well = position,
             Name = sample,
             Type = sample.type,
             "Input Units" = quantity,
             "RFU ~ Cycle" = slope,
             # "E" = coefE,
             "Pred. Units" = punits,
             "Pred. Units Mean" = punitsText,
             # "Pred. Units Mean" = meanPunits,
             # "Pred. Units ± Mean (PI)" = meanPunitsPI,
             "Dilution N" = dilutionN,
             "Dilution" = dilution,
             # "Stock Units" = stockUnits,
             # "Stock Units ± (PI)" = stockUnitsPI
             "Stock Units" = sunitsText
      )
  })

  output$unknSmplUI <- renderUI({
    req(punitsResultsTbl())
    snames <- unique(punitsResultsTbl() %>%
                       filter(sample.type == "unkn") %>%
                       .$sampleName)
    if (length(snames) == 0)
      snames <- unique(punitsResultsTbl()$sampleName)
    selectInput("unknSmplSlct",
                "Show samples",
                snames)
  })

  output$resultsTable <- DT::renderDataTable({
    # req(results())
    req(input$unknSmplSlct)
    cat("making resultsTable\n")
    results() %>%
      DT::datatable(
        data = .,
        extensions = 'Buttons',
        options = list(paging = FALSE,
                       dom = 'Bfrtip',
                       buttons = c('copy', 'csv', 'excel')),
        selection = "multiple")
  })

  output$curvePlot <- renderPlotly({
    req(input$resultsTable_rows_selected)
    cat("drawing curvePlot\n")
    # selected <- punitsResultsTbl() %>%
    #   filter(position %in%
    #            punitsResultsTbl()[input$resultsTable_rows_selected, "position"]) %>%
    #   as.data.frame()
    selected <- punitsResultsTbl()[input$resultsTable_rows_selected, ]
    curveNames <- selected[["fdata.name"]]

    fdt <- lapply(curveNames, function(curveName) {
      data.frame(cyc = fData()[["cyc"]],
                 fluor = fData()[[curveName]],
                 sname = curveName)
    })
    fdt <- bind_rows(fdt)

    slopeLine <- lapply(curveNames, function(curveName) {
      slopeRegion <- {
        if (input$useGlobalSlope)
          input$slopeRegion
        else {
          sname <- description()[fdata.name == curveName, c(sample)]
          input[[paste0("slopeRegion_", sname)]]
        }
      }
      prediction <- predict(mlist()[[curveName]])
      data.frame(cyc = slopeRegion,
                 fluor = c(prediction[1], tail(prediction, 1)),
                 sname = curveName)

    })
    slopeLine <- bind_rows(slopeLine)

    ggplot(fdt) +
      geom_line(aes(x = cyc, y = fluor, color = sname)) +
      geom_line(data = slopeLine, aes(x = cyc, y = fluor, group = sname), color = "black") +
      theme_bw() +
      theme(legend.title=element_blank())
  })

  output$meltPlot <- renderPlotly({
    req(input$resultsTable_rows_selected)
    cat("drawing meltPlot\n")
    selected <- punitsResultsTbl()[input$resultsTable_rows_selected, ]
    meltData <- tryCatch(
      rdmlFile()$GetFData(selected, dp.type = "mdp", long.table = TRUE),
      error = function(e) NULL)
    req(meltData)
    meltData <- meltData %>%
      group_by(fdata.name) %>%
      mutate(diffFluor := c(0, diff(fluor) * -1))
    ggplot(meltData, aes(x = tmp, y = diffFluor, color = fdata.name)) +
      geom_line() +
      theme_bw() +
      theme(legend.position = "none")
  })

  output$calibrationPlot <- renderPlotly({
    req(punitsResultsTbl(),
        input$unknSmplSlct)
    cat("drawing calibrationPlot\n")
    # input$quantitiesSlct
    stdTbl <- punitsResultsTbl() %>%
      filter(sample.type == "std")
    unknTbl <- punitsResultsTbl() %>%
      filter(sample.type == "unkn" & sampleName == input$unknSmplSlct)
    selectedQuantities <- as.numeric(input$quantitiesSlct)
    p <- {
      switch(input$calibrationParameter,
             "slope" =  ggplot(stdTbl %>%
                                 filter(quantity %in% selectedQuantities),
                               aes(x = quantity,
                                   y = slope)) +
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
                          shape = 24, fill = "orange", size = 3),
             "end point" = ggplot(stdTbl %>%
                                    filter(quantity %in% selectedQuantities),
                                  aes(x = quantity,
                                      y = endPointRFU)) +
               geom_smooth(method = "lm", color = "red", fill = "red") +
               geom_point(size = 3, aes(group = sample)) +
               geom_point(data = stdTbl %>%
                            filter(!(quantity %in% selectedQuantities)),
                          color = "grey60", size = 3, aes(group = sample)) +
               geom_point(data = unknTbl %>%
                            filter(punits <= max(selectedQuantities) &
                                     punits >= min(selectedQuantities)),
                          aes(x = punits, y = endPointRFU, group = sample),
                          shape = 24, fill = "green4", size = 3) +
               geom_point(data = unknTbl %>%
                            filter(punits > max(selectedQuantities) |
                                     punits < min(selectedQuantities)),
                          aes(x = punits, y = endPointRFU, group = sample),
                          shape = 24, fill = "orange", size = 3)
      )
    }
    p + theme_bw()
  })
})
