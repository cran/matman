# Shiny app server function
#
# @param input provided by shiny
# @param output provided by shiny
#
.shinyAppServer <-
  function(input, output) {

    ##### data sets #####

    output$data_Amount <- DT::renderDataTable({

      Amount

    }, rownames = F)

    output$data_Stocks <- DT::renderDataTable({

      Stocks

    }, rownames = F)
    #####

    ##### aggreageData #####

    output$aggDatEnd_Amount <- DT::renderDataTable({

      agg <- switch(input$agdatfun,
                    sum = sum,
                    mean = mean)

      aggregateData(data = Amount,
                    value = input$agdatval,
                    item = input$agdatit,
                    timestamp = "date",
                    temporalAggregation = input$agdatAg,
                    fiscal = input$agdatfisc,
                    aggregationFun = agg)

    },rownames=F)


    output$aggDatFormel_Amount <- renderText({

      paste0('aggregateData(
                data = Amount,
                value = "', input$agdatval, '",
                item = "', list(input$agdatit), '",
                timestamp = "date",
                temporalAggregation = "', input$agdatAg, '",
                fiscal = ', input$agdatfisc, ',
                aggregationFun = ', input$agdatfun,'
             )')

    })
    #####

    #####  extendData  #####

    output$exdatexp1<-renderUI({

      if(input$exdatdat == "Amount"){
        pickerInput("exdatexp", "expand column", choices = c("item", "itemgroup"), selected="item", multiple = T)
      }
      else if(input$exdatdat == "Stocks"){
        pickerInput("exdatexp", "expand column", choices = c("item"), selected="item", multiple = T)
      }
    })


    output$exdatval1<-renderUI({

      if(input$exdatdat == "Amount"){
        pickerInput("exdatval", "value column", choices = c("amount", "value"), selected="amount", multiple = T)
      }
      else if(input$exdatdat == "Stocks"){
        pickerInput("exdatval", "value column", choices = c("stock","minstock","reorderlevel"), selected="stock", multiple = T)
      }
    })

    output$exdatstamp1<-renderUI({

      if(input$exdatdat == "Amount"){
        selectInput("exdatstamp", "timestamp", choices = c("date","week","month","quarter","year"), selected="date")
      }
      else if(input$exdatdat == "Stocks"){
        selectInput("exdatstamp", "timestamp", choices = "date", selected="date")
      }
    })

    output$exdatstampformat1<-renderUI({

      if(input$exdatdat == "Amount"){
        if(input$exdatstamp == "date"){
          selectInput("exdatstampformat", "timestampFormat", choices = "day", selected="day")
        } else
          if(input$exdatstamp == "week"){
            selectInput("exdatstampformat", "timestampFormat", choices = "week", selected="week")
          } else
            if(input$exdatstamp == "month"){
              selectInput("exdatstampformat", "timestampFormat", choices = "month", selected="month")
            } else
              if(input$exdatstamp == "quarter"){
                selectInput("exdatstampformat", "timestampFormat", choices = "quarter", selected="quarter")
              } else
                if(input$exdatstamp == "year"){
                  selectInput("exdatstampformat", "timestampFormat", choices = "year", selected="year")
                }
      }
      else if(input$exdatdat == "Stocks"){
        selectInput("exdatstampformat", " timestampFormat", choices = "day", selected="date")
      }
    })

    output$exDatFormel <- renderText({

      paste0('expandData(
                data = ', input$exdatdat, '
                expand = "', list(input$exdatexp), '",
                expandTo = "', input$exdatexpto, '",
                valueColumns = "', list(input$exdatval), '",
                latest_values = ', input$exdatlatest, ',
                valueLevels = "', input$exdatlevel, '",
                timestamp = "', input$exdatstamp, '",
                timestampFormat = "', input$exdatstampformat, '",
                keepData = ', input$exdatkeep,'
             )')

    })



    output$exDatEnd <- DT::renderDataTable({

      dats <- switch(input$exdatdat,
                     Amount = Amount,
                     Stocks = Stocks)


      expandData(
        data = dats,
        expand = input$exdatexp,
        expandTo = input$exdatexpto,
        valueColumns = input$exdatval,
        latest_values = input$exdatlatest,
        valueLevels = input$exdatlevel,
        timestamp = input$exdatstamp,
        timestampFormat = input$exdatstampformat,
        keepData = input$exdatkeep
      )


    },rownames=F)

    #####

    ##### ABC - Analysis #####

    output$ABCBC1 <- renderUI({
      mini <- input$ABCAB+1

      sliderInput("ABCBC", "threshold BC", min = mini, max = 100, value = 95, step = 1)

    })


    ABC<- reactive({
      computeABCXYZAnalysis(data = Amount,
                            value = input$ABCval,
                            item = input$ABCit,
                            timestamp = "date",
                            AB = input$ABCAB,
                            BC = input$ABCBC
      )
    })


    output$ABCFormel <- renderText({

      paste0('result = computeABCXYZAnalysis(
                          data = Amount,
                          value = "', input$ABCval, '",
                          item = "', list(input$ABCit), '",
                          timestamp = "date",
                          AB = ', input$ABCAB,'
                          BC = ', input$ABCBC,'
                          )')

    })

    output$ABCEnd<- DT::renderDataTable({

      show(ABC())

    },rownames=F)

    output$ABCsumm<- DT::renderDataTable({

      summary(ABC(), withMissing = input$ABCwMiss)

    },rownames=F)

    output$ABCsummFormel<- renderText({

      paste0("summary(result, withMissing = ", input$ABCwMiss, ")")

    })

    output$ABCplot<- renderPlot({

      plot(ABC(), plot_engine="graphics")

    })

    ABC1 <- reactive({

      data1 = Amount[which(Amount$date %in% c(input$range1[1]:input$range1[2])),]
      data2 = Amount[which(Amount$date %in% c(input$range2[1]:input$range2[2])),]

      abcxyzData1<- computeABCXYZAnalysis(data = data1,
                                          value = input$ABCval,
                                          item = input$ABCit,
                                          timestamp = "date",
                                          AB = input$ABCAB,
                                          BC = input$ABCBC
      )

      abcxyzData2<- computeABCXYZAnalysis(data = data2,
                                          value = input$ABCval,
                                          item = input$ABCit,
                                          timestamp = "date",
                                          AB = input$ABCAB,
                                          BC = input$ABCBC,
      )


      uneqABC <- switch(input$ABCunequalABC,
                        "NA" = NA,
                        "FALSE" = FALSE,
                        "TRUE" = TRUE)


      compare(abcxyzData1,
              abcxyzData2,
              valueDiff = input$ABCcompvaldif,
              unequalABC = uneqABC
      )
    })

    output$ABCcompFormel <- renderText({

      paste0('result1 = compare(
                          object1,
                          object2,
                          valueDiff = ', input$ABCcompvaldif, ',
                          unequalABC = ', input$ABCunequalABC, '
                          )')

    })

    output$ABCcompare <- DT::renderDataTable({

      show(ABC1())
    }, rownames = F)

    output$ABCcompsummFormel<- renderText({

      paste0("summary(result1, withMissing = ", input$ABCcompwMiss, ")")

    })

    output$ABCcomparesumm <- DT::renderDataTable({

      wMiss<- switch(input$ABCcompwMiss,
                     "FALSE" = FALSE,
                     "TRUE" = TRUE)

      as.data.frame.matrix(summary(ABC1(), withMissing = wMiss))

    }, rownames=T)

    #####

    ##### ABCXYZ - Analysis #####

    output$ABCXYZBC1 <- renderUI({
      mini <- input$ABCXYZAB+1

      sliderInput("ABCXYZBC", "threshold BC", min = mini, max = 100, value = 95, step = 1)

    })

    output$XYZYZ1 <- renderUI({
      mini <- input$XYZXY+0.1

      sliderInput("XYZYZ", "threshold YZ", min = mini, max = 3, value = 1, step = 0.1)

    })

    ABCXYZ<- reactive({
      computeABCXYZAnalysis(data = Amount,
                            value = input$ABCXYZval,
                            item = input$ABCXYZit,
                            timestamp = "date",
                            temporalAggregation = input$ABCXYZAg,
                            AB = input$ABCXYZAB,
                            BC = input$ABCXYZBC,
                            XY = input$XYZXY,
                            YZ = input$XYZYZ,
                            ignoreZeros = input$ABCXYZig0
      )
    })


    output$ABCXYZFormel <- renderText({

      paste0('result = computeABCXYZAnalysis(
                          data = Amount,
                          value = "', input$ABCXYZval, '",
                          item = "', list(input$ABCXYZit), '",
                          timestamp = "date",
                          temporalAggregation = "', input$ABCXYZAg, '",
                          ignoreZeros = ', input$ABCXYZig0,'
                          AB = ', input$ABCXYZAB,'
                          BC = ', input$ABCXYZBC,'
                          XY = ', input$XYZXY,'
                          YZ = ', input$XYZYZ,'
                          )')

    })

    output$ABCXYZEnd<- DT::renderDataTable({

      show(ABCXYZ())

    },rownames=F)

    output$ABCXYZsumm<- DT::renderDataTable({

      summary(ABCXYZ(), withMissing = input$ABCXYZwMiss)

    },rownames=F)

    output$ABCXYZsummFormel<- renderText({

      paste0("summary(result, withMissing = ", input$ABCXYZwMiss, ")")

    })

    output$ABCXYZplot<- renderPlot({

      plot(ABCXYZ(), plot_engine="graphics")

    })

    ABCXYZ1 <- reactive({

      data1 = Amount[which(Amount$date %in% c(input$XYZrange1[1]:input$XYZrange1[2])),]
      data2 = Amount[which(Amount$date %in% c(input$XYZrange2[1]:input$XYZrange2[2])),]

      abcxyzData1<- computeABCXYZAnalysis(data = data1,
                                          value = input$ABCXYZval,
                                          item = input$ABCXYZit,
                                          timestamp = "date",
                                          temporalAggregation = input$ABCXYZAg,
                                          AB = input$ABCXYZAB,
                                          BC = input$ABCXYZBC,
                                          XY = input$XYZXY,
                                          YZ = input$XYZYZ,
                                          ignoreZeros = input$ABCXYZig0
      )

      abcxyzData2<- computeABCXYZAnalysis(data = data2,
                                          value = input$ABCXYZval,
                                          item = input$ABCXYZit,
                                          timestamp = "date",
                                          temporalAggregation = input$ABCXYZAg,
                                          AB = input$ABCXYZAB,
                                          BC = input$ABCXYZBC,
                                          XY = input$XYZXY,
                                          YZ = input$XYZYZ,
                                          ignoreZeros = input$ABCXYZig0
      )


      uneqABCXYZ <- switch(input$ABCXYZunequalABC,
                           "NA" = NA,
                           "FALSE" = FALSE,
                           "TRUE" = TRUE)
      uneqXYZ <- switch(input$ABCXYZunequalXYZ,
                        "NA" = NA,
                        "FALSE" = FALSE,
                        "TRUE" = TRUE)


      compare(abcxyzData1,
              abcxyzData2,
              valueDiff = input$ABCXYZcompvaldif,
              xyzCoefficientDiff = input$ABCXYZcompcoefdif,
              unequalABC = uneqABCXYZ,
              unequalXYZ = uneqXYZ
      )
    })

    output$ABCXYZcompFormel <- renderText({

      paste0('result1 = compare(
                          object1,
                          object2,
                          valueDiff = ', input$ABCXYZcompvaldif, ',
                          xyzCoefficientDiff = ', input$ABCXYZcompcoefdif, ',
                          unequalABC = ', input$ABCXYZunequalABC, ',
                          unequalXYZ = ', input$ABCXYZunequalXYZ,'
                          )')

    })

    output$ABCXYZcompare <- DT::renderDataTable({

      show(ABCXYZ1())
    }, rownames = F)

    output$ABCXYZcompsummFormel<- renderText({

      paste0("summary(result1, withMissing = ", input$ABCXYZcompwMiss, ")")

    })

    output$ABCXYZcomparesumm <- DT::renderDataTable({

      wMiss<- switch(input$ABCXYZcompwMiss,
                     "FALSE" = FALSE,
                     "TRUE" = TRUE)

      as.data.frame.matrix(summary(ABCXYZ1(), withMissing = wMiss))

    }, rownames=T)

    #####

    ##### Performerfunctions ########

    output$performthresval1 <- renderUI({

      if(input$performthres == "column"){
        selectInput("performthresval", "threshold value", choices = c("minstock", "reorderlevel"), selected = "reorderlevel")
      } else if(input$performthres == "value"){
        numericInput("performthresval", "threshold value", value = 0)
      }
    })

    output$over <- DT::renderDataTable({

      computeOverperformer(data = Stocks,
                           value = input$performvalue,
                           group = input$performgroup,
                           timestamp = input$performstamp,
                           timestampFormat = input$performstampFormat,
                           currentTime = input$performdate,
                           thresholdValue = input$performthresval,
                           use_latest = input$performlatest,
                           thresholdTime = input$performthrestime)
    }, rownames = F)

    output$overFormel <- renderText({

      paste0('computeOverperformer(
                          data = Stocks,
                          value = "', input$performvalue, '",
                          group = "', list(input$performgroup), '",
                          timestamp = "date",
                          timestampFormat = "', input$performstampFormat, '",
                          currentTime = ', input$performdate,'
                          thresholdValue = "', input$performthresval,'",
                          use_latest = ', input$performlatest,',
                          thresholdTime = ', input$performthrestime,'
                          )')

    })

    output$under <- DT::renderDataTable({

      computeUnderperformer(data = Stocks,
                            value = input$performvalue,
                            group = input$performgroup,
                            timestamp = input$performstamp,
                            timestampFormat = input$performstampFormat,
                            currentTime = input$performdate,
                            thresholdValue = input$performthresval,
                            use_latest = input$performlatest,
                            thresholdTime = input$performthrestime)
    }, rownames = F)

    output$underFormel <- renderText({

      paste0('computeUnderperformer(
                          data = Stocks,
                          value = "', input$performvalue, '",
                          group = "', list(input$performgroup), '",
                          timestamp = "date",
                          timestampFormat = "', input$performstampFormat, '",
                          currentTime = ', input$performdate,'
                          thresholdValue = "', input$performthresval,'"
                          use_latest = ', input$performlatest,',
                          thresholdTime = ', input$performthrestime,'
                          )')

    })


    output$const <- DT::renderDataTable({

      computeConstants(data = Stocks,
                       value = input$performvalue,
                       group = input$performgroup,
                       timestamp = "date",
                       timestampFormat = input$performstampFormat,
                       currentTime = input$performdate,
                       thresholdTime = input$performthrestime)
    }, rownames = F)

    output$constFormel <- renderText({

      paste0('computeConstants(
                          data = Stocks,
                          value = "', input$performvalue, '",
                          group = "', list(input$performgroup), '",
                          timestamp = "date",
                          timestampFormat = "', input$performstampFormat, '",
                          currentTime = ', input$performdate,'
                          thresholdTime = ', input$performthrestime,'
                          )')

    })

    #####

    ##### predictValue #####

    predVal <- reactive({

      agg <- switch(input$preddatfun,
                    sum = sum,
                    mean = mean)

      predictValue(data = Amount,
                   value = input$preddatval,
                   item = input$preddatit,
                   timestamp = "date",
                   temporalAggregation = input$preddatAg,
                   aggregationFun = agg,
                   timeUnitsAhead = input$preddatahead,
                   digits = input$preddatdigits,
                   expand = input$preddatexp,
                   keepPreviousData = input$preddatkeep,
                   level = input$preddatlevel
      )
    })

    output$predDatFormel <- renderText({

      paste0('result = predictValue(
                          data = Amount,
                          value = "', input$preddatval, '",
                          item = "', list(input$preddatit), '",
                          timestamp = "date",
                          temporalAggregation = "', input$preddatAg, '",
                          aggregationFun = ', input$preddatfun,'
                          timeUnitsAhead = ', input$preddatahead,'
                          digits = ', input$preddatdigits,'
                          expand = ', input$preddatexp,'
                          keepPreviousData = ', input$preddatkeep,'
                          level = ', input$preddatlevel,'
                          )')

    })


    output$predDatEnd <- DT::renderDataTable({

      predVal()@data

    },rownames=F)

    output$predDatSumm <- DT::renderDataTable({

      summary(predVal())

    },rownames=F)


    #####

    ##### detectTimeVariations #####

    timeVar <- reactive({

      agg <- switch(input$timevardatfun,
                    sum = sum,
                    mean = mean)

      detectTimeVariations(data = Amount,
                           value = input$timevardatval,
                           item = input$timevardatit,
                           timestamp = "date",
                           temporalAggregation = input$timevardatAg,
                           aggregationFun = agg,
                           preProcess = input$timevardatpre,
                           recentTimePeriods = input$timevardatrecent

      )
    })

    output$timevarDatFormel <- renderText({

      paste0('detectTimeVariations(
                          data = Amount,
                          value = "', input$timevardatval, '",
                          item = "', list(input$timevardatit), '",
                          timestamp = "date",
                          temporalAggregation = "', input$timevardatAg, '",
                          aggregationFun = ', input$timevardatfun,'
                          preProcess = ', input$timevardatpre,'
                          recentTimePeriods = ', input$timevardatrecent,'
                          )')

    })

    output$timeVarDatEnd <- DT::renderDataTable({

      timeVar()


    },rownames=F)

    #####

    ##### plotValueSeries #####

    valueSeries <- reactive({

      lt <- switch(input$valsertrendtype,
                   "simple" = "s",
                   "weighted" = "w")


      plotValueSeries(data = Amount,
                      item = input$valserit,
                      item_id = input$valseritid,
                      value = input$valserval,
                      timestamp = input$valserstamp,
                      temporalAggregation = input$valseragg,
                      expand = input$valserex,
                      withTrendLine = input$valsertrend,
                      windowLength = input$valserwinlen,
                      trendLineType = lt
      )

    })

    output$valserFormel <- renderText({

      paste0('plotValueSeries(
                          data = Amount,
                          item = "', input$valserit, '",
                          item_id = "', input$valseritid, '",
                          value = "', input$valserval, '",
                          timestamp = "date",
                          temporalAggregation = "', input$valseragg, '",
                          expand = ', input$valserex,'
                          withTrendLine = ', input$valsertrend,'
                          windowLength = ', input$valserwinlen,'
                          trendLineType = "', input$valsertrendtype,'"
                          )')

    })

    output$valserEnd <- renderPlotly({

      valueSeries()


    })

    #####

  }
