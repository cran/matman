.shinyAppUI <- function() {
  
  ui = dashboardPage(
    dashboardHeader(
      title = "Examples matman"
    ),
    
    dashboardSidebar(
      sidebarMenu(
        br(),
        menuItem("data sets", tabName = "dats"),
        br(),
        p("Functions:"),
        menuItem("aggregateData", tabName = "agdat"),
        menuItem("expandData", tabName = "exdat"),
        menuItem("computeABC", tabName = "compABC"),
        menuItem("computeABCXYZ", tabName = "compABCXYZ"),
        menuItem("performerfunctions", tabName = "perform"),
        menuItem("predictValue", tabName = "pred"),
        menuItem("detectTimeVariations", tabName = "timevar"),
        menuItem("plotValueSeries", tabName = "valser")
      )
    ),
    
    dashboardBody(
      
      tabItems(
        
        ######## data sets #########
        tabItem(tabName = "dats",
                tabBox(title = "Data Sets", width = 12,
                       tabPanel("Amount",
                                DT::dataTableOutput("data_Amount")
                       ),
                       tabPanel("Stocks",
                                DT::dataTableOutput("data_Stocks")
                       )
                )
        ),
        
        ########
        
        ########  aggregateData ########
        
        tabItem(tabName = "agdat",
                fluidRow(
                  box(width=9, title = "Parameters:", solidHeader =T, status ="primary", collapsible = T,
                      fluidRow(
                        column(4,
                               selectInput("agdatdat", "data set",choices="Amount", selected ="Amount")
                        ),
                        column(4,
                               radioButtons("agdatval", "value column", choices = c("amount", "value"), selected="amount", inline = T)
                        ),
                        column(4,
                               pickerInput("agdatit", "item columns", choices = c("item", "itemgroup"), selected = "item", multiple = T)
                        )
                      ),
                      fluidRow(
                        column(3,
                               selectInput("agdatstamp", "timestamp", choices = "date", selected ="date")
                        ),
                        column(3,
                               selectInput("agdatAg", "temporal Aggregation", choices= c("day", "week", "month", "quarter", "year"))
                        ),
                        column(3,
                               radioButtons("agdatfun", "aggregation function", choices = c("sum", "mean"), selected="sum", inline = T)
                        ),
                        column(3,
                               numericInput("agdatfisc", "fiscal month", value = 1, min = 1, max = 12)
                        )
                      )
                  ),
                  box(width = 3, title = "Function", solidHeader =T, status ="primary", collapsible = T,
                      textOutput("aggDatFormel_Amount"),
                      tags$style(type="text/css", "#aggDatFormel_Amount {white-space: pre-wrap;}")
                  )
                  
                ),
                fluidRow(
                  box(width = 12, title = "Result:", solidHeader =T, status ="primary",
                      DT::dataTableOutput("aggDatEnd_Amount"), style = "overflow-y: scroll;overflow-x: scroll;"
                  )
                )
        ),
        ########
        
        ######## expandData #########
        
        tabItem(tabName = "exdat",
                fluidRow(
                  box(width=9, title = "Parameters:", solidHeader =T, status ="primary", collapsible = T,
                      fluidRow(
                        column(width = 3,
                               radioButtons("exdatdat", "data set", choices = c("Amount", "Stocks"), selected="Amount", inline = T)
                        ),
                        column(width = 2,
                               uiOutput("exdatexp1")
                        ),
                        column(width = 2,
                               radioButtons("exdatexpto", "expand", choices = c("all", "event"), selected="all", inline = T)
                        ),
                        column(width = 2,
                               uiOutput("exdatval1")
                        ),
                        column(width = 2,
                               radioButtons("exdatlatest", "repeat latest", choices = c("TRUE", "FALSE"), selected="FALSE", inline = T)
                        )
                      ),
                      fluidRow(
                        column(width = 3,
                               textInput("exdatlevel", "value levels", value="NA")
                        ),
                        column(width = 3,
                               uiOutput("exdatstamp1")
                        ),
                        column(width = 3,
                               uiOutput("exdatstampformat1")
                        ),
                        column(width = 3,
                               radioButtons("exdatkeep", "keep data", choices = c("TRUE", "FALSE"), selected="FALSE", inline = T)
                        )
                      )
                  ),
                  box(width = 3, title = "Function", solidHeader =T, status ="primary", collapsible =T,
                      textOutput("exDatFormel"),
                      tags$style(type="text/css", "#exDatFormel {white-space: pre-wrap;}")
                  )
                ),
                fluidRow(
                  box(width =12 , title = "Result", solidHeader =T, status ="primary",
                      DT::dataTableOutput("exDatEnd"), style = "overflow-y: scroll;overflow-x: scroll;"
                  )
                )
        ),
        ########
        
        ######## compute ABC ##########
        
        tabItem(tabName = "compABC",
                fluidRow(
                  tabBox(width = 12, title = "Functions",
                         tabPanel("compute",
                                  fluidRow(
                                    box(width=9, title = "Parameters:", solidHeader =T, status ="primary", collapsible = T,
                                        fluidRow(
                                          column(3,
                                                 selectInput("ABCdat", "data set", choices= "Amount", selected = "Amount")
                                          ),
                                          column(3,
                                                 radioButtons("ABCval", "value column", choices = c("amount", "value"), selected="amount")
                                          ),
                                          column(3,
                                                 pickerInput("ABCit", "item columns", choices = c("item", "itemgroup"), selected = "item", multiple = T)
                                          ),
                                          column(3,
                                                 selectInput("ABCdate", "timestamp", choices= "date", selected ="date")
                                          )
                                        ),
                                        fluidRow(
                                          column(6,
                                                 sliderInput("ABCAB", "threshold AB", min = 0, max = 100, value = 80, step = 1)
                                          ),
                                          column(6,
                                                 uiOutput("ABCBC1")
                                          )
                                        )
                                    ),
                                    box( width = 3, title = "Function", status = "primary", solidHeader = T, collapsible = T,
                                         textOutput("ABCFormel"),
                                         tags$style(type="text/css", "#ABCFormel {white-space: pre-wrap;}")
                                    )
                                    
                                  ),
                                  fluidRow(
                                    box(width = 12, title = "Results", solidHeader = T, status = "primary",
                                        tabBox(width= 12, title = "Methods",
                                               tabPanel("show",
                                                        box(width =12,
                                                            p(strong("Code:"),"show(result)")
                                                        ),
                                                        DT::dataTableOutput("ABCEnd")
                                               ),
                                               tabPanel("summary",
                                                        box(width = 6,
                                                            radioButtons("ABCwMiss", "with Missing classes:", choices = c(TRUE, FALSE), selected = TRUE, inline = TRUE)
                                                        ),
                                                        box(width =6,
                                                            p(strong("Code: ")),
                                                            textOutput("ABCsummFormel")
                                                        ),
                                                        DT::dataTableOutput("ABCsumm")
                                               ),
                                               tabPanel("plot",
                                                        fluidRow(
                                                          box(width =12,
                                                              p(strong("Code:"),"plot(result, plot_engine = 'plot')")
                                                          )
                                                        ),
                                                        fluidRow(
                                                          plotOutput("ABCplot")
                                                        )
                                               )
                                        )
                                    )
                                  )
                         ),
                         
                         tabPanel("compare",
                                  fluidRow(
                                    box(width = 9, title = "Parameters:", status = "primary", solidHeader = T, collapsible = T,
                                        fluidRow(
                                          column(width=3,
                                                 dateRangeInput("range1", "date range (object1)", min = min(Amount$date), max = max(Amount$date),
                                                                start = "2017-08-01", end = "2018-07-31")
                                          ),
                                          column(width=3,
                                                 dateRangeInput("range2", "date range (object2)", min = min(Amount$date), max = max(Amount$date),
                                                                start = "2018-08-01", end = "2019-07-31")
                                          ),
                                          column(width=3,
                                                 numericInput("ABCcompvaldif", "value diff", value = 0, min = 0)
                                          ),
                                          column(width=3,
                                                 radioButtons("ABCunequalABC", "unequalABC", choices = c("NA", "TRUE", "FALSE"), selected = "NA", inline =T)
                                          )
                                        )
                                    ),
                                    box(width = 3, title = "Function:", status = "primary", solidHeader = T, collapsible = T,
                                        textOutput("ABCcompFormel"),
                                        tags$style(type="text/css", "#ABCcompFormel {white-space: pre-wrap;}"))
                                  ),
                                  fluidRow(
                                    box(width = 12, title = "Results", status = "primary", solidHeader = T,
                                        tabBox(width= 12, title = "Methods",
                                               tabPanel("show",
                                                        box(width =12,
                                                            p(strong("Code:"),"show(result1)")
                                                        ),
                                                        DT::dataTableOutput("ABCcompare"), style = "overflow-y: scroll;overflow-x: scroll;"
                                               ),
                                               tabPanel("compare summary",
                                                        box(width = 6,
                                                            radioButtons("ABCcompwMiss", "with Missing classes:", choices = c(TRUE, FALSE), selected = TRUE, inline = TRUE)
                                                        ),
                                                        box(width =6,
                                                            p(strong("Code: ")),
                                                            textOutput("ABCcompsummFormel")
                                                        ),
                                                        DT::dataTableOutput("ABCcomparesumm")
                                               )
                                        )
                                    )
                                  )
                         )
                  )
                )
        ),
        
        ########
        
        ######## compute ABCXYZ ##########
        
        tabItem(tabName = "compABCXYZ",
                fluidRow(
                  tabBox(width = 12, title = "Functions",
                         tabPanel("compute",
                                  fluidRow(
                                    box(width=9, title = "Parameters:", solidHeader =T, status ="primary", collapsible = T,
                                        fluidRow(
                                          column(2,
                                                 selectInput("ABCXYZdat", "data set", choices= "Amount", selected = "Amount")
                                          ),
                                          column(2,
                                                 radioButtons("ABCXYZval", "value column", choices = c("amount", "value"), selected="amount")
                                          ),
                                          column(2,
                                                 pickerInput("ABCXYZit", "item columns", choices = c("item", "itemgroup"), selected = "item", multiple = T)
                                          ),
                                          column(2,
                                                 selectInput("ABCXYZdate", "timestamp", choices= "date", selected ="date")
                                          ),
                                          column(2,
                                                 selectInput("ABCXYZAg", "temp. Aggregation", choices= c("day", "week", "month", "quarter", "year"), selected ="day")
                                          ),
                                          column(2,
                                                 radioButtons("ABCXYZig0", "ignore Zeros", choices = c("TRUE", "FALSE"), selected="TRUE")
                                          )
                                        ),
                                        fluidRow(
                                          column(3,
                                                 sliderInput("ABCXYZAB", "threshold AB", min = 0, max = 100, value = 80, step = 1)
                                          ),
                                          column(3,
                                                 uiOutput("ABCXYZBC1")
                                          ),
                                          column(3,
                                                 sliderInput("XYZXY", "threshold XY", min = 0, max = 10, value = 0.5, step = 0.1)
                                          ),
                                          column(3,
                                                 uiOutput("XYZYZ1")
                                          )
                                        )
                                    ),
                                    box( width = 3, title = "Function", status = "primary", solidHeader = T, collapsible = T,
                                         textOutput("ABCXYZFormel"),
                                         tags$style(type="text/css", "#ABCXYZFormel {white-space: pre-wrap;}")
                                    )
                                    
                                  ),
                                  fluidRow(
                                    box(width = 12, title = "Results", solidHeader = T, status = "primary",
                                        tabBox(width= 12, title = "Methods",
                                               tabPanel("show",
                                                        box(width =12,
                                                            p(strong("Code:"),"show(result)")
                                                        ),
                                                        DT::dataTableOutput("ABCXYZEnd")
                                               ),
                                               tabPanel("summary",
                                                        box(width = 6,
                                                            radioButtons("ABCXYZwMiss", "with Missing classes:", choices = c(TRUE, FALSE), selected = TRUE, inline = TRUE)
                                                        ),
                                                        box(width =6,
                                                            p(strong("Code: ")),
                                                            textOutput("ABCXYZsummFormel")
                                                        ),
                                                        DT::dataTableOutput("ABCXYZsumm")
                                               ),
                                               tabPanel("plot",
                                                        fluidRow(
                                                          box(width =12,
                                                              p(strong("Code:"),"plot(result, plot_engine = 'plot')")
                                                          )
                                                        ),
                                                        fluidRow(
                                                          plotOutput("ABCXYZplot")
                                                        )
                                               )
                                        )
                                    )
                                  )
                         ),
                         
                         tabPanel("compare",
                                  fluidRow(
                                    box(width = 9, title = "Parameters:", status = "primary", solidHeader = T, collapsible = T,
                                        fluidRow(
                                          column(width=6,
                                                 dateRangeInput("XYZrange1", "date range (object1)", min = min(Amount$date), max = max(Amount$date),
                                                                start = "2017-08-01", end = "2018-07-31")
                                          ),
                                          column(width=6,
                                                 dateRangeInput("XYZrange2", "date range (object2)", min = min(Amount$date), max = max(Amount$date),
                                                                start = "2018-08-01", end = "2019-07-31")
                                          ),
                                        ),
                                        fluidRow(
                                          column(width=3,
                                                 numericInput("ABCXYZcompvaldif", "value diff", value = 0, min = 0)
                                          ),
                                          column(width=3,
                                                 numericInput("ABCXYZcompcoefdif", "xyzCoefficientDiff", value = 0, min = 0, step = 0.1)
                                          ),
                                          column(width=3,
                                                 radioButtons("ABCXYZunequalABC", "unequalABC", choices = c("NA", "TRUE", "FALSE"), selected = "NA", inline =T)
                                          ),
                                          column(width=3,
                                                 radioButtons("ABCXYZunequalXYZ", "unequalXYZ", choices = c("NA", "TRUE", "FALSE"), selected = "NA", inline =T)
                                          )
                                        )
                                    ),
                                    box(width = 3, title = "Function:", status = "primary", solidHeader = T, collapsible = T,
                                        textOutput("ABCXYZcompFormel"),
                                        tags$style(type="text/css", "#ABCXYZcompFormel {white-space: pre-wrap;}"))
                                  ),
                                  fluidRow(
                                    box(width = 12, title = "Results", status = "primary", solidHeader = T,
                                        tabBox(width= 12, title = "Methods",
                                               tabPanel("show",
                                                        box(width =12,
                                                            p(strong("Code:"),"show(result1)")
                                                        ),
                                                        DT::dataTableOutput("ABCXYZcompare"), style = "overflow-y: scroll;overflow-x: scroll;"
                                               ),
                                               tabPanel("compare summary",
                                                        box(width = 6,
                                                            radioButtons("ABCXYZcompwMiss", "with Missing classes:", choices = c(TRUE, FALSE), selected = TRUE, inline = TRUE)
                                                        ),
                                                        box(width =6,
                                                            p(strong("Code: ")),
                                                            textOutput("ABCXYZcompsummFormel")
                                                        ),
                                                        DT::dataTableOutput("ABCXYZcomparesumm")
                                               )
                                        )
                                    )
                                  )
                         )
                  )
                )
        ),
        
        ########
        
        ######## Performer ##########
        
        tabItem(tabName = "perform",
                fluidRow(
                  box(width=12, title = "Parameters:", solidHeader = T, status = "primary", collapsible = T,
                      fluidRow(
                        column(width = 3,
                               selectInput("performdat", "data set", choices = "Stocks", selected = "Stocks")
                        ),
                        column(width = 2,
                               selectInput("performvalue", "value variable", choices = "stock", selected = "stock")
                        ),
                        column(width = 2,
                               selectInput("performgroup", "group", choices = "item", selected = "item")
                        ),
                        column(width = 2,
                               selectInput("performstamp", "timestamp", choices = "date", selected = "date")
                        ),
                        column(width = 3,
                               selectInput("performstampFormat", "timestamp Format", choices = "day", selected = "day")
                        )
                      ),
                      fluidRow(
                        column(width = 3,
                               dateInput("performdate", "currentTime", max= max(Stocks$date), value = max(Stocks$date))
                        ),
                        column(width = 2,
                               radioButtons("performthres", "threshold", choices=c("column", "value"), selected="column", inline = T)
                        ),
                        column(width = 2,
                               uiOutput("performthresval1")
                        ),
                        column(width = 2,
                               radioButtons("performlatest", "use latest value", choices=c("TRUE", "FALSE"), selected="TRUE", inline = T)
                        ),
                        column(width = 3,
                               numericInput("performthrestime", "threshold time", min=1, value = 90, step = 1 )
                        )
                      )
                      
                  )
                ),
                
                fluidRow(
                  tabBox(width = 12, title = "Functions",
                         tabPanel("computeOverperformer",
                                  fluidRow(
                                    box(width = 3, title = "Function", solidHeader = T, status = "primary",
                                        textOutput("overFormel"),
                                        tags$style(type="text/css", "#overFormel {white-space: pre-wrap;}")
                                    ),
                                    box(width = 9, title = "Result", solidHeader = T, status = "primary",
                                        DT::dataTableOutput("over")
                                    )
                                  )
                         ),
                         tabPanel("computeUnderperformer",
                                  fluidRow(
                                    box(width = 3, title = "Function", solidHeader = T, status = "primary",
                                        textOutput("underFormel"),
                                        tags$style(type="text/css", "#underFormel {white-space: pre-wrap;}")
                                    ),
                                    box(width = 9, title = "Result", solidHeader = T, status = "primary",
                                        DT::dataTableOutput("under")
                                    )
                                  )
                         ),
                         tabPanel("computeConstants",
                                  fluidRow(
                                    box(width = 3, title = "Function", solidHeader = T, status = "primary",
                                        textOutput("constFormel"),
                                        tags$style(type="text/css", "#constFormel {white-space: pre-wrap;}")
                                    ),
                                    box(width = 9, title = "Result", solidHeader = T, status = "primary",
                                        DT::dataTableOutput("const")
                                    )
                                  )
                         )
                  )
                )
        ),
        ########
        
        ######## predictValue ##########
        
        tabItem(tabName = "pred",
                fluidRow(
                  box(width=9, title = "Parameters:", solidHeader = T, status = "primary", collapsible = T,
                      fluidRow(
                        column(width = 2,
                               selectInput("preddatdat", "data set", choices = "Amount", selected = "Amount")),
                        column(width = 2,
                               radioButtons("preddatval", "value column", choices = c("amount", "value"), selected="amount")
                        ),
                        column(width = 2,
                               selectInput("preddatit", "item columns", choices = c("item", "itemgroup"), selected = "item")
                        ),
                        column(width = 2,
                               selectInput("preddattime", "timestamp", choices = "date", selected = "date")
                        ),
                        column(width = 2,
                               selectInput("preddatAg", "temp. Aggregation", choices= c("day", "week", "month", "quarter", "year"))
                        ),
                        column(width = 2,
                               radioButtons("preddatfun", "agg. function", choices = c("sum", "mean"), selected="sum")
                        )
                      ),
                      fluidRow(
                        column(width = 2, offset =1,
                               numericInput("preddatahead", "time units ahead", value = 3, min = 1, step = 1)
                        ),
                        column(width = 2,
                               numericInput("preddatdigits", "digits", value = 5, min = 0, step = 1)
                        ),
                        column(width = 2,
                               radioButtons("preddatexp", "expand data", choices = c("TRUE", "FALSE"), selected="FALSE")
                        ),
                        column(width = 2,
                               radioButtons("preddatkeep", "keep data", choices = c("TRUE", "FALSE"), selected="FALSE")),
                        column(width = 2,
                               numericInput("preddatlevel", "level", value = 0.95, min = 0, max = 1, step = 0.01)
                        )
                      )
                  ),
                  box(width = 3, title = "Function:", solidHeader = T, status = "primary", collapsible = T,
                      textOutput("predDatFormel"),
                      tags$style(type="text/css", "#predDatFormel {white-space: pre-wrap;}")
                  )
                ),
                fluidRow(
                  box(width = 12, title = "Result", solidHeader = T, status = "primary", collapsible = T,
                      tabBox(width = 12,
                             tabPanel("show",
                                      box(width =12,
                                          p(strong("Code:"),"show(result)")
                                      ),
                                      DT::dataTableOutput("predDatEnd"), style = "overflow-y: scroll;overflow-x: scroll;"),
                             tabPanel("summary",
                                      box(width =12,
                                          p(strong("Code:"),"summary(result)")
                                      ),
                                      DT::dataTableOutput("predDatSumm"), style = "overflow-y: scroll;overflow-x: scroll;")
                      )
                  )
                )
                
                
        ),
        ########
        
        ######## timeVariations ##########
        
        tabItem(tabName = "timevar",
                fluidRow(
                  box(width=9, title = "Parameters:", solidHeader =T, status = "primary", collapsible = T,
                      fluidRow(
                        column(width = 3,
                               selectInput("timevardatdat", "data set", choices = "Amount", selected = "Amount")
                        ),
                        column(width = 3,
                               radioButtons("timevardatval", "value column", choices = c("amount", "value"), selected="amount", inline = T)
                        ),
                        column(width = 3,
                               selectInput("timevardatit", "item columns", choices = c("item", "itemgroup"), selected = "item"),
                        ),
                        column(width = 3,
                               selectInput("timevardattime", "timestamp", choices = "date", selected = "date")
                        )
                      ),
                      fluidRow(
                        column(width = 3,
                               selectInput("timevardatAg", "temporal Aggregation", choices= c("day", "week", "month", "quarter", "year"))
                        ),
                        column(width = 3,
                               radioButtons("timevardatfun", "aggregation function", choices = c("sum", "mean"), selected="sum", inline = T)
                        ),
                        column(width = 3,
                               radioButtons("timevardatpre", "preProcess", choices = c(NA, "center"), selected=NA, inline = T)
                        ),
                        column(width = 3,
                               numericInput("timevardatrecent", "recent time Periods", value = 5, min = 5, step = 1)
                        )
                        
                      )
                  ),
                  box(width = 3, title = "Function:", solidHeader =T, status = "primary", collapsible = T,
                      textOutput("timevarDatFormel"),
                      tags$style(type="text/css", "#timevarDatFormel {white-space: pre-wrap;}")
                  )
                ),
                fluidRow(
                  box(width = 12, title = "Result", solidHeader =T, status = "primary",
                      DT::dataTableOutput("timeVarDatEnd"), style = "overflow-y: scroll;overflow-x: scroll;"
                  )
                )
        ),
        ########
        
        ######## plotValueSeries ########
        
        tabItem(tabName = "valser",
                fluidRow(
                  box(width=9, title = "Parameters:", solidHeader =T, status = "primary", collapsible = T,
                      fluidRow(
                        column(width = 3,
                               selectInput("valserdat", "data set", choices = "Amount", selected = "Amount")
                        ),
                        column(width = 2,
                               selectInput("valserit", "item column", choices = "item", selected="item")
                        ),
                        column(width = 2,
                               selectInput("valseritid", "item id", choices = sort(unique(Amount$item)), selected = "2008"),
                        ),
                        column(width = 2,
                               selectInput("valserval", "value column", choices = c("amount", "value"), selected = "amount")
                        ),
                        column(width = 3,
                               selectInput("valserstamp", "timestamp", choices = "date", selected = "date")
                        )
                      ),
                      fluidRow(
                        column(width = 3,
                               selectInput("valseragg", "temp. Aggregation", choices= c("day", "week", "month", "quarter", "year"), selected = "week")
                        ),
                        column(width = 2,
                               radioButtons("valserex", "expand", choices = c("TRUE", "FALSE"), selected= "TRUE", inline = T)
                        ),
                        column(width = 2,
                               radioButtons("valsertrend", "trendline", choices = c("TRUE", "FALSE"), selected= "TRUE", inline = T)
                        ),
                        column(width = 2,
                               numericInput("valserwinlen", "windowLength", value = 5, min = 1, step = 1)
                        ),
                        column(width = 3,
                               radioButtons("valsertrendtype", "trendlinetype", choices = c("simple", "weighted"), selected= "simple", inline = T)
                        )
                        
                      )
                  ),
                  box(width = 3, title = "Function:", solidHeader =T, status = "primary", collapsible = T,
                      textOutput("valserFormel"),
                      tags$style(type="text/css", "#valserFormel {white-space: pre-wrap;}")
                  )
                ),
                fluidRow(
                  box(width = 12, title = "Result", solidHeader =T, status = "primary",
                      plotlyOutput("valserEnd"), style = "overflow-y: scroll;overflow-x: scroll;"
                  )
                  
                )
        )
        ########
      ),
    ),
    skin = "blue"
  )
  
  return(ui)
}
