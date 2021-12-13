#' Performs a temporal aggregation of a data frame
#'
#' Aggregates a data frame based on a timestamp column to days, weeks, months, quarters, years or total.
#'
#' @param data                  Data frame or matrix on which the ABC analysis is performed.
#' @param value                 Name(s) of the column variable(s) that contains the values for the ABC and XYZ analysis.
#' @param item                  Names of the columns including the item names or identifiers (e.g., product name, EAN).
#' @param timestamp             Name of the column including the timestamp. This column should be in POSIX or Date-format.
#' @param temporalAggregation   Temporal aggregation mode for the XYZ-analysis. Possible modes are 'day', 'week', 'month', 'quarter', 'year',
#' and 'total'. Total only aggregates by item whereas the other modes aggregate by item an temporal unit.
#' @param fiscal                consider the start of the business year. Default is set to 1 (January)
#' @param aggregationFun        Function for aggregating the value column. Default is \code{sum}.
#' @return Returns a data frame with the aggregated data with the columns of item, timestamp and sum,
#' which is the sum of the value column.
#' @seealso \code{\link{expandData}}
#' @author Leon Binder \email{leon.binder@@th-deg.de}
#' @author Bernhard Bauer \email{bernhard.bauer@@th-deg.de}
#' @author Michael Scholz \email{michael.scholz@@th-deg.de}
#' @examples
#' data('Amount')
#' aggregatedData = aggregateData(data = Amount,
#'     value = "value",
#'     item = "item",
#'     timestamp = "date",
#'     temporalAggregation = "quarter")
#' @export
aggregateData = function(data,
                         value=NULL,
                         item,
                         timestamp,
                         temporalAggregation = c("day", "week", "month", "quarter", "year", "total"),
                         fiscal = 1,
                         aggregationFun = sum) {
  if (!all(value %in% names(data))) {
    stop(paste("Data does not include column ", value, ".", sep = ""))
  }
  if (!all(item %in% names(data))) {
    stop(paste("Data does not include all of the columns: ", item, ".", sep = ""))
  }
  if (!timestamp %in% names(data)) {
    stop(paste("Data does not include column ", timestamp, ".", sep = ""))
  }
  
  # Conversion from POSIX to Date.
  # UNTESTED FOR DIFFERENT FORMATS! MAYBE AN ARGUMENT timestampFormat IS NECESSARY?
  if (!inherits(data[[timestamp]], 'Date')){
    data[[timestamp]] = as.Date(as.character(data[[timestamp]]))
  }
  
  # If the business year does not start in January week, month, quarter and year are shifted to the start of business year
  if (temporalAggregation!="day" & fiscal > 1 ) {
    data[[timestamp]]<- as.POSIXlt(data[[timestamp]])
    data[[timestamp]]$mon<- data[[timestamp]]$mon + (12-fiscal+1)
    data[[timestamp]] <- as.Date(data[[timestamp]])
  }
  
  # Aggregation based on temporalAggregation.
  if (temporalAggregation == "day") {
    data[[temporalAggregation]] = data[[timestamp]]
  } else if (temporalAggregation == "week") {
    data[[temporalAggregation]] = paste(isoyear(data[[timestamp]]), sprintf("%02d", isoweek(data[[timestamp]])), sep = "-W")
  } else if (temporalAggregation == "month") {
    data[[temporalAggregation]] = paste(year(data[[timestamp]]), sprintf("%02d", month(data[[timestamp]])), sep = "-")
  } else if (temporalAggregation == "quarter") {
    data[[temporalAggregation]] = paste(year(data[[timestamp]]), quarter(data[[timestamp]]), sep = "-Q")
  } else if (temporalAggregation == "year") {
    data[[temporalAggregation]] = year(data[[timestamp]])
  } else if (temporalAggregation == "total") {
    data[[temporalAggregation]] <- "total"
  }
  
  # Shrink data to relevant variables
  data <- data[,c(temporalAggregation, item, value)]
  
  help1 <- c(temporalAggregation, item)
  aggregatedData = as.data.table(data)[,lapply(.SD, aggregationFun), keyby = eval(get("help1"))]
  
  return(data.frame(aggregatedData))
}



#' Expands a temporal data frame
#'
#' Expands a temporal data frame and fills values for missing dates.
#'
#' @param data Data frame that will be expanded.
#' @param expand Name of the variables that will be expanded.
#' @param expandTo Defines whether values for the variables to be expanded will be filled for all dates or only those dates included in the data.
#' @param valueColumns Name of the columns that are filled with specific values.
#' @param latest_values If True missing values are filled with the latest known value until the next known value comes in.
#' @param valueLevels Specific values that are used to fill the value columns. If latest_values = TRUE only values with no known values in the past of
#' this values are specified with this specific values.
#' @param timestamp Name of the column including the timestamp. This column should be in Date , YY-mm, YYYY-'W'ww, YYYY-mm, YYYY-'Q'q or YYYY format.
#' @param timestampFormat Declares in which format the timestamp comes in (i.e., "day", "week", "month", "quarter", "year").
#' @param keepData Defines whether variables that will not be expanded should be kept.
#' @return Returns the expanded data frame.
#' @seealso \code{\link{aggregateData}}
#' @author Leon Binder \email{leon.binder@@th-deg.de}
#' @author Bernhard Bauer \email{bernhard.bauer@@th-deg.de}
#' @author Michael Scholz \email{michael.scholz@@th-deg.de}
#' @examples
#' data("Amount")
#' expandedItems = expandData(Amount,
#'     expand = c("item", "itemgroup"),
#'     expandTo = "all",
#'     valueColumns = c("amount", "value"),
#'     latest_values = TRUE,
#'     valueLevels = c(0, 0),
#'     timestamp = "date",
#'     timestampFormat = "day")
#' @export
expandData = function(data,
                      expand,
                      expandTo = c("all", "event"),
                      valueColumns,
                      latest_values = F,
                      valueLevels = NA,
                      timestamp,
                      timestampFormat = c("day", "week", "month", "quarter", "year"),
                      keepData = T
) {
  
  ### Bring timestamp to daily format
  if (timestampFormat %in% c("day", "week", "month", "year")) {
    data[[timestamp]] = as.Date(parse_iso_8601(data[[timestamp]]))
  }
  else if (timestampFormat == "quarter") {
    help<-as.numeric(substr(data[[timestamp]],7,7))*3
    data[[timestamp]] = paste0(substr(data[[timestamp]],1,4), "-",help, "-01")
    data[[timestamp]] = as.Date(data[[timestamp]], format = "%Y-%m-%d")
  }
  
  
  ### Extend and join the data
  if (expandTo == "all") {
    dates = data.frame(stamp = seq(min(data[[timestamp]], na.rm = TRUE), max(data[[timestamp]], na.rm = TRUE), by = timestampFormat))
  } else if (expandTo == "event") {
    dates = data.frame(stamp=sort(unique(data[[timestamp]])))
  }
  
  names(dates) = timestamp
  extend = data %>% select(all_of(expand)) %>% distinct()
  datesextend = crossing(dates, extend)
  fulldata = full_join(datesextend, data)
  
  #### Fill data
  for (i in 1:length(valueColumns)) {
    valueColumn = valueColumns[i]
    
    ### with latest values
    if(length(latest_values) == 1){
      if(latest_values){
        fulldata <- fulldata %>% group_by(across(expand)) %>% fill(!!as.name(valueColumn))
      }
    } else {
      if(latest_values[i]){
        fulldata <- fulldata %>% group_by(across(expand)) %>% fill(!!as.name(valueColumn))
      }
    }
    
    ### rest with valueLevels
    if(length(valueLevels)>1){
      fulldata[which(is.na(fulldata[, valueColumn])), valueColumn] = valueLevels[i]
    } else{
      fulldata[which(is.na(fulldata[, valueColumn])), valueColumn] = valueLevels
    }
  }
  
  fulldata <- as.data.frame(fulldata)
  
  if (keepData == F) {
    fulldata = fulldata[, c(timestamp, expand, valueColumns)]
  }
  
  #### Bring timestamp in original format
  if (timestampFormat == "week") {
    fulldata[[timestamp]] = paste(isoyear(fulldata[[timestamp]]), sprintf("%02d", isoweek(fulldata[[timestamp]])), sep = "-W")
  } else if (timestampFormat == "month") {
    fulldata[[timestamp]] = paste(year(fulldata[[timestamp]]), sprintf("%02d", month(fulldata[[timestamp]])), sep = "-")
  } else if (timestampFormat == "quarter") {
    fulldata[[timestamp]] = paste0(year(fulldata[[timestamp]]), "-Q", quarter(fulldata[[timestamp]]))
  } else if (timestampFormat == "year") {
    fulldata[[timestamp]] = year(fulldata[[timestamp]])
  }
  fulldata <- setorderv(fulldata,expand)
  return(fulldata)
}




