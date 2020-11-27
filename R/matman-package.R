#' Material Management
#'
#' A set of functions, classes and methods for performing ABC and ABC/XYZ analyses, identifying overperforming, underperforming and
# 'constantly performing items, and plotting, analyzing as well as predicting the temporal development of items.
#'
#' \tabular{ll}{ Package: \tab matman\cr Type: \tab Package\cr Version:
#' \tab 1.1.2\cr Date: \tab 2020-11-27\cr License: \tab GPL-3\cr Depends: \tab
#' R (>= 3.5.0), stats\cr }
#'
#' @name matman-package
#' @aliases matman-package matman
#' @docType package
#' @author Leon Binder \email{leon.binder@@th-deg.de}
#' @author Bernhard Bauer \email{bernhard.bauer@@th-deg.de}
#' @author Michael Scholz \email{michael.scholz@@th-deg.de}
#'
#' @import methods
#' @import data.table
#' @import tidyverse
#' @import shinydashboard
#' @importFrom shiny br column fluidPage fluidRow numericInput reactive renderPlot renderText renderUI selectInput shinyApp sliderInput tabPanel uiOutput p radioButtons textOutput tags textInput strong plotOutput dateRangeInput dateInput
#' @importFrom shinyWidgets pickerInput
#' @importFrom graphics axis lines plot text
#' @importFrom stats dist xtabs predict qnorm fitted setNames
#' @importFrom utils head tail
#' @importFrom dplyr %>% distinct full_join left_join group_by inner_join n select summarize mutate_all
#' @importFrom tidyr crossing spread fill
#' @importFrom forecast auto.arima
#' @importFrom ISOweek ISOweek ISOweek2date
#' @importFrom plotly renderPlotly plotlyOutput
#' @importFrom DT renderDataTable dataTableOutput
#' @concept ABC-Analysis
#' @concept XYZ-Analysis
NULL
