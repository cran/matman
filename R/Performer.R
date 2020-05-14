
# Returns the maxima of the input values. This function does not show a warning if the vector
# of input values is of length 0.
.robustMax = function(x, na.rm = TRUE) {
  if (length(x)>0) {
    return(max(x, na.rm = na.rm))
  } else {
    return(-Inf)
  }
}

#' Select underperforming items
#'
#' Selects items with a value lower than a given threshold for a specified time period.
#'
#' @param data Dataframe containing item stock data.
#' @param group Name(s) of the column(s) that are used to group stock data. These columns are usually the item ID or item name to group
#' stock data by items.
#' @param value Name of the column variable containing the stock values.
#' @param timestamp  Name of the column including the timestamp. This column should be in Date, POSIX , YY-mm, YYYY-'W'ww, YYYY-mm, YYYY-'Q'q or YYYY format.
#' @param timestampFormat Declares in which format the timestamp comes in (i.e., "day", "week", "month", "quarter", "year")
#' @param currentTime Qualifying date for the value variable. Date must exist in data and have the same format as timestamp-variable.
#' @param thresholdValue Name of the colum variable containing the items' stock threshold value or the threshold value used in this analysis
#' for all items.
#' @param thresholdTime Time for which the value shouldn't exceed the threshold value. Number declares the time in the format of timestampFormat
#' @param use_latest boolean value. If TRUE data will expand and dates with noexisting values will be filled up with the latest known values
#' @return Returns a data frame listing all underperforming items, the date their stock was the last time over the threshold (lastover),
#' the duration in days since the stock is under the threshold (toolowindays), the average difference between the stock and the threshold
#' (meandiff) and the count of switched between over- and underperformance (moves).
#' @seealso \code{\link{computeOverperformer}} \code{\link{computeConstants}}
#' @author Leon Binder \email{leon.binder@@th-deg.de}
#' @author Bernhard Bauer \email{bernhard.bauer@@th-deg.de}
#' @author Michael Scholz \email{michael.scholz@@th-deg.de}
#' @examples
#' data("Stocks")
#' underperformer = computeUnderperformer(data=Stocks,
#'                                        value = "stock",
#'                                        group = "item",
#'                                        timestamp = "date",
#'                                        timestampFormat = "day",
#'                                        currentTime = "2019-07-27",
#'                                        thresholdValue = "minstock",
#'                                        thresholdTime = 90,
#'                                        use_latest = FALSE)
#' @export
computeUnderperformer = function(data,
                                 value,
                                 group,
                                 timestamp,
                                 timestampFormat = c("day", "week", "month", "quater","year"),
                                 currentTime,
                                 thresholdValue = 0,
                                 thresholdTime = 90,
                                 use_latest = FALSE) {


  if(use_latest == TRUE & !is.numeric(thresholdValue)){
    data<-expandData(data = data,
                     expand = group,
                     expandTo = "all",
                     valueColumns = c(value,thresholdValue), latest_values = T,timestamp = timestamp, timestampFormat = timestampFormat, keepData = F)
  }

  if(use_latest == TRUE & is.numeric(thresholdValue)){
    data<-expandData(data = data,
                     expand = group,
                     expandTo = "all",
                     valueColumns = value,
                     latest_values = T,
                     timestamp = timestamp,
                     timestampFormat = timestampFormat,
                     keepData = F)
  }

  ### Bring timestamp and currentTime to date format
  if (timestampFormat == "day") {
    data[[timestamp]] = as.Date(as.character(data[[timestamp]]))
  }
  else if (timestampFormat == "week") {
    data[[timestamp]] = paste0(data[[timestamp]], "-1")
    data[[timestamp]] = ISOweek2date(data[[timestamp]])
    currentTime = paste0(currentTime, "-1")
    currentTime<-ISOweek2date(currentTime)
  }
  else if (timestampFormat == "month") {
    data[[timestamp]] = paste0(data[[timestamp]], "-01")
    data[[timestamp]] = as.Date(data[[timestamp]], format = "%Y-%m-%d")
    currentTime = paste0(currentTime, "-01")
    currentTime<-as.Date(currentTime, format = "%Y-%m-%d")
  }
  else if (timestampFormat == "quarter") {
    data[[timestamp]] = paste0(substr(data[[timestamp]],1,4), "-", as.numeric(substr(data[[timestamp]],7,7))*3, "-01")
    data[[timestamp]] = as.Date(data[[timestamp]], format = "%Y-%m-%d")
    currentTime = paste0(substr(currentTime,1,4), "-", as.numeric(substr(currentTime,7,7))*3, "-01")
    currentTime<-as.Date(currentTime, format = "%Y-%m-%d")
  }
  else if (timestampFormat == "year") {
    data[[timestamp]] = paste0(data[[timestamp]], "-01-01")
    data[[timestamp]] = as.Date(data[[timestamp]], format = "%Y-%m-%d")
    currentTime = paste0(currentTime, "-01-01")
    currentTime = as.Date(currentTime, format = "%Y-%m-%d")
  }


  ### Select only Data in the past of currentTime
  data = data[data[[timestamp]] <= currentTime, ]

  ### Determine last timestamp when thresholdValue was overtaken
  if (!is.numeric(thresholdValue)) {
    currentdata = data[which(data[, timestamp] == currentTime), c(group, value, timestamp, thresholdValue)]
    lastexceed = setDT(data)[, list(
      lastover = .robustMax(c(min(get(timestamp), na.rm = T), get(timestamp)[get(..value) > get(thresholdValue)])),
      mindate = .robustMax(min(get(timestamp), na.rm = T)),
      moves = length(rle(get(..value)[get(timestamp) > .robustMax(get(timestamp)[get(..value) > get(thresholdValue)])])$length),
      meandiff = round(mean(get(thresholdValue)[get(timestamp) > .robustMax(get(timestamp)[get(..value) > get(thresholdValue)])] -
                              get(..value)[get(timestamp) > .robustMax(get(timestamp)[get(..value) > get(thresholdValue)])], na.rm = T),digits = 2)
    ),
    by = eval(get("group"))]
  } else {
    currentdata = data[which(data[, timestamp] == currentTime), c(group, value, timestamp)]
    currentdata$minimumStock = thresholdValue
    lastexceed = setDT(data)[, list(
      lastover = .robustMax(c(min(get(timestamp), na.rm = T), get(timestamp)[get(..value) > thresholdValue])),
      mindate = .robustMax(min(get(timestamp), na.rm = T)),
      moves = length(rle(get(..value)[get(timestamp) > .robustMax(get(timestamp)[get(..value) > thresholdValue])])$length),
      meandiff = round(mean(thresholdValue - get(..value)[get(timestamp) > .robustMax(get(timestamp)[get(..value) > thresholdValue])], na.rm = T),digits = 2)
    ),
    by = eval(get("group"))]
  }

  result = left_join(currentdata, lastexceed, by = group)
  result = result[order(result$lastover, -result$moves), ]

  ### Subset data for condition thresholdTime and determine timespan in aggregation of timestampFormat
  if(timestampFormat == "day"){
    result = result[result$lastover <= as.Date(currentTime) - thresholdTime, ]
    if(nrow(result)>0){
      result$toolowindays = result[[timestamp]] - result$lastover
    }
  }
  else if(timestampFormat =="week"){
    result = result[result$lastover <= as.Date(currentTime) - as.difftime(thresholdTime, units="weeks"), ]
    if(nrow(result)>0){
      result$toolowinweeks = paste((result[[timestamp]] - result$lastover)/7,"weeks")
      result$lastover = ISOweek(result$lastover)
      result[[timestamp]] = ISOweek(result[[timestamp]])
    }
  }
  else if(timestampFormat == "month"){
    result = result[result$lastover <= seq(as.Date(currentTime), length = 2, by = paste0("-",thresholdTime," months"))[2], ]
    if(nrow(result)>0){
      result$toolowinmonth = paste((year(result[[timestamp]]) - year(result$lastover))*12 + (month(result[[timestamp]]) - month(result$lastover)),"month")
      result$lastover = format(result$lastover, "%Y-%m")
      result[[timestamp]] = format(result[[timestamp]], "%Y-%m")
    }
  }
  else if(timestampFormat == "quarter"){
    result = result[result$lastover <= seq(as.Date(currentTime), length = 2, by = paste0("-",thresholdTime," quarters"))[2], ]
    if(nrow(result)>0){
      result$toolowinquarters = paste((year(result[[timestamp]]) - year(result$lastover))*4 + (quarter(result[[timestamp]]) - quarter(result$lastover)),"quaters")
      result$lastover = paste0(year(result$lastover), "-Q", quarter(result$lastover))
      result[[timestamp]] = paste0(year(result[[timestamp]]), "-Q", quarter(result[[timestamp]]))
    }
  }
  else if(timestampFormat == "year"){
    result = result[result$lastover <= seq(as.Date(currentTime), length = 2, by = paste0("-",thresholdTime," years"))[2], ]
    if(nrow(result)>0){
      result$toolowinyears = paste(year(result[[timestamp]]) - year(result$lastover),"years")
      result$lastover = year(result$lastover)
      result[[timestamp]] = year(result[[timestamp]])
    }
  }

  if(any(result$lastover == result$mindate )){

    equals <- which(result$lastover == result$mindate)

    result$lastover<- as.character(result$lastover)
    result$lastover[equals] <- paste(">=", result$lastover[equals])
    result[, ncol(result)]<- as.character( result[, ncol(result)])
    result[equals, ncol(result)] <- paste(">=", result[equals, ncol(result)])
  }
  result$mindate <- NULL

  return(result)
}




#' Select overperforming items
#'
#' Selects items with a value higher than a given threshold for a specified time period.
#'
#' @param data Dataframe containing item stock data.
#' @param group Name(s) of the column(s) that are used to group stock data. These columns are usually the item ID or item name to group
#' stock data by items.
#' @param value Name of the column variable containing the stock values.
#' @param timestamp  Name of the column including the timestamp. This column should be in Date, POSIX , YY-mm, YYYY-'W'ww, YYYY-mm, YYYY-'Q'q or YYYY format.
#' @param timestampFormat Declares in which format the timestamp comes in (i.e., "day", "week", "month", "quarter", "year")
#' @param currentTime Qualifying date for the value variable. Date must exist in data and have the same format as timestamp-variable.
#' @param thresholdValue Name of the colum variable containing the items' stock threshold value or the threshold value used in this analysis
#' for all items.
#' @param thresholdTime Time for which the value shouldn't exceed the threshold value. Number declares the time in the format of timestampFormat.
#' @param use_latest boolean value. If TRUE data will expand and dates with noexisting values will be filled up with the latest known values.
#' @return Returns a data frame listing all overperforming items, the date their stock was the last time under the threshold (lastunder),
#' the duration in days since the stock is over the threshold (toolowindays), the average difference between the stock and the threshold
#' (meandiff) and the count of switched between over- and underperformance (moves).
#' @seealso \code{\link{computeUnderperformer}} \code{\link{computeConstants}}
#' @author Leon Binder \email{leon.binder@@th-deg.de}
#' @author Bernhard Bauer \email{bernhard.bauer@@th-deg.de}
#' @author Michael Scholz \email{michael.scholz@@th-deg.de}
#' @examples
#' data("Stocks")
#' overperformer = computeOverperformer(data = Stocks,
#'                      value = "stock",
#'                      group = "item",
#'                      timestamp = "date",
#'                      timestampFormat = "day",
#'                      currentTime = "2019-07-27",
#'                      thresholdValue = "reorderlevel",
#'                      thresholdTime = 0,
#'                      use_latest = FALSE)
#' @export
computeOverperformer = function(data,
                                value,
                                group,
                                timestamp,
                                timestampFormat = c("day", "week", "month", "quater","year"),
                                currentTime,
                                thresholdValue = 0,
                                thresholdTime = 90,
                                use_latest = FALSE) {


  if(use_latest == TRUE & !is.numeric(thresholdValue)){
    data<-expandData(data = data,
                     expand = group,
                     expandTo = "all",
                     valueColumns = c(value,thresholdValue), latest_values = T,timestamp = timestamp, timestampFormat = timestampFormat, keepData = F)
  }

  if(use_latest == TRUE & is.numeric(thresholdValue)){
    data<-expandData(data = data,
                     expand = group,
                     expandTo = "all",
                     valueColumns = value,
                     latest_values = T,
                     timestamp = timestamp,
                     timestampFormat = timestampFormat,
                     keepData = F)
  }


  ### Bring timestamp and currentTime to date format
  if (timestampFormat == "day") {
    data[[timestamp]] = as.Date(as.character(data[[timestamp]]))
  }
  else if (timestampFormat == "week") {
    data[[timestamp]] = paste0(data[[timestamp]], "-1")
    data[[timestamp]] = ISOweek2date(data[[timestamp]])
    currentTime = paste0(currentTime, "-1")
    currentTime<-ISOweek2date(currentTime)
  }
  else if (timestampFormat == "month") {
    data[[timestamp]] = paste0(data[[timestamp]], "-01")
    data[[timestamp]] = as.Date(data[[timestamp]], format = "%Y-%m-%d")
    currentTime = paste0(currentTime, "-01")
    currentTime<-as.Date(currentTime, format = "%Y-%m-%d")
  }
  else if (timestampFormat == "quarter") {
    data[[timestamp]] = paste0(substr(data[[timestamp]],1,4), "-", as.numeric(substr(data[[timestamp]],7,7))*3, "-01")
    data[[timestamp]] = as.Date(data[[timestamp]], format = "%Y-%m-%d")
    currentTime = paste0(substr(currentTime,1,4), "-", as.numeric(substr(currentTime,7,7))*3, "-01")
    currentTime<-as.Date(currentTime, format = "%Y-%m-%d")
  }
  else if (timestampFormat == "year") {
    data[[timestamp]] = paste0(data[[timestamp]], "-01-01")
    data[[timestamp]] = as.Date(data[[timestamp]], format = "%Y-%m-%d")
    currentTime = paste0(currentTime, "-01-01")
    currentTime = as.Date(currentTime, format = "%Y-%m-%d")
  }

  ### Select only Data in the past of currentTime
  data = data[data[[timestamp]] <= currentTime, ]

  ### Determine last timestamp when thresholdValue was overtaken
  if (!is.numeric(thresholdValue)) {
    currentdata = data[which(data[, timestamp] == currentTime), c(group, value, timestamp, thresholdValue)]
    lastfallshort = setDT(data)[, list(
      lastunder = .robustMax(c(min(get(timestamp), na.rm = T), get(timestamp)[get(..value) < get(thresholdValue)])),
      mindate = .robustMax(min(get(timestamp), na.rm = T)),
      moves = length(rle(get(..value)[get(timestamp) > .robustMax(get(timestamp)[get(..value) < get(thresholdValue)], na.rm = T)])$length),
      meandiff = round(mean(get(..value)[get(timestamp) > .robustMax(get(timestamp)[get(..value) < get(thresholdValue)])] -
                              get(thresholdValue)[get(timestamp) > .robustMax(get(timestamp)[get(..value) < get(thresholdValue)])]), digits = 2)
    ),
    by = eval(get("group"))]
  } else {
    currentdata = data[which(data[, timestamp] == currentTime), c(group, value, timestamp)]
    currentdata$thresholdValue = thresholdValue
    lastfallshort = setDT(data)[, list(
      lastunder = .robustMax(c(min(get(timestamp), na.rm = T), get(timestamp)[get(..value) < thresholdValue])),
      mindate = .robustMax(min(get(timestamp), na.rm = T)),
      moves = length(rle(get(..value)[get(timestamp) > .robustMax(get(timestamp)[get(..value) < thresholdValue], na.rm = T)])$length),
      meandiff = round(mean(get(..value)[get(timestamp) > .robustMax(get(timestamp)[get(..value) < thresholdValue])] - thresholdValue), digits = 2)
    ),
    by = eval(get("group"))]
  }

  result = left_join(currentdata, lastfallshort, by = group)
  result = result[order(result$lastunder, -result$moves), ]

  ### Subset data for condition thresholdTime and determine timespan in aggregation of timestampFormat
  if(timestampFormat == "day"){
    result = result[result$lastunder <= as.Date(currentTime) - thresholdTime, ]
    if(nrow(result)>0){
      result$toohighindays = result[[timestamp]] - result$lastunder
    }
  }
  else if(timestampFormat =="week"){
    result = result[result$lastunder <= as.Date(currentTime) - as.difftime(thresholdTime, units="weeks"), ]
    if(nrow(result)>0){
      result$toohighinweeks = paste((result[[timestamp]] - result$lastunder)/7,"weeks")
      result$lastunder = ISOweek(result$lastunder)
      result[[timestamp]] = ISOweek(result[[timestamp]])
    }
  }
  else if(timestampFormat == "month"){
    result = result[result$lastunder <= seq(as.Date(currentTime), length = 2, by = paste0("-",thresholdTime," months"))[2], ]
    if(nrow(result)>0){
      result$toohighinmonth = paste((year(result[[timestamp]]) - year(result$lastunder))*12 + (month(result[[timestamp]]) - month(result$lastunder)),"month")
      result$lastunder = format(result$lastunder, "%Y-%m")
      result[[timestamp]] = format(result[[timestamp]], "%Y-%m")
    }
  }
  else if(timestampFormat == "quarter"){
    result = result[result$lastunder <= seq(as.Date(currentTime), length = 2, by = paste0("-",thresholdTime," quarters"))[2], ]
    if(nrow(result)>0){
      result$toohighinquarters = paste((year(result[[timestamp]]) - year(result$lastunder))*4 + (quarter(result[[timestamp]]) - quarter(result$lastunder)),"quaters")
      result$lastunder = paste0(year(result$lastunder), "-Q", quarter(result$lastunder))
      result[[timestamp]] = paste0(year(result[[timestamp]]), "-Q", quarter(result[[timestamp]]))
    }
  }
  else if(timestampFormat == "year"){
    result = result[result$lastunder <= seq(as.Date(currentTime), length = 2, by = paste0("-",thresholdTime," years"))[2], ]
    if(nrow(result)>0){
      result$toohighinyears = paste(year(result[[timestamp]]) - year(result$lastunder),"years")
      result$lastunder = year(result$lastunder)
      result[[timestamp]] = year(result[[timestamp]])
    }
  }

  if(any(result$lastunder == result$mindate )){

    equals <- which(result$lastunder == result$mindate)

    result$lastunder<- as.character(result$lastunder)
    result$lastunder[equals] <- paste(">=", result$lastunder[equals])
    result[, ncol(result)]<- as.character( result[, ncol(result)])
    result[equals, ncol(result)] <- paste(">=", result[equals, ncol(result)])
  }
  result$mindate <- NULL


  return(result)
}


#' Select constant items
#'
#' Selects items with a constant value for a specified time period.
#'
#' @param data Dataframe containing item stock data.
#' @param group Name(s) of the column(s) that are used to group stock data. These columns are usually the item ID or item name to group
#' stock data by items.
#' @param value Name of the column variable containing the stock values.
#' @param timestamp  Name of the column including the timestamp. This column should be in Date, POSIX , YY-mm, YYYY-'W'ww, YYYY-mm, YYYY-'Q'q or YYYY format.
#' @param timestampFormat Declares in which format the timestamp comes in (i.e., "day", "week", "month", "quarter", "year")
#' @param currentTime Qualifying date for the value variable. Date must exist in data and have the same format as timestamp-variable.
#' @param thresholdTime Time for which the value shouldn't exceed the threshold value. Number declares the time in the format of timestampFormat.
#' @return Returns a data frame listing all constant items, the date since when the stock is constant and the value of the stock since this time.
#' @seealso \code{\link{computeUnderperformer}} \code{\link{computeOverperformer}}
#' @author Leon Binder \email{leon.binder@@th-deg.de}
#' @author Bernhard Bauer \email{bernhard.bauer@@th-deg.de}
#' @author Michael Scholz \email{michael.scholz@@th-deg.de}
#' @examples
#' data("Stocks")
#' constants = computeConstants(data=Stocks,
#'                              value = "stock",
#'                              group = "item",
#'                              timestamp = "date",
#'                              timestampFormat = "day",
#'                              currentTime = "2019-07-27",
#'                              thresholdTime = 7)
#' @export
computeConstants = function(data,
                            value,
                            group,
                            timestamp,
                            timestampFormat = c("day", "week", "month", "quater","year"),
                            currentTime,
                            thresholdTime = 7) {


  ### Bring timestamp and currentTime to date format
  if (timestampFormat == "day") {
    data[[timestamp]] = as.Date(as.character(data[[timestamp]]))
  }
  else if (timestampFormat == "week") {
    data[[timestamp]] = paste0(data[[timestamp]], "-1")
    data[[timestamp]] = ISOweek2date(data[[timestamp]])
    currentTime = paste0(currentTime, "-1")
    currentTime<-ISOweek2date(currentTime)
  }
  else if (timestampFormat == "month") {
    data[[timestamp]] = paste0(data[[timestamp]], "-01")
    data[[timestamp]] = as.Date(data[[timestamp]], format = "%Y-%m-%d")
    currentTime = paste0(currentTime, "-01")
    currentTime<-as.Date(currentTime, format = "%Y-%m-%d")
  }
  else if (timestampFormat == "quarter") {
    data[[timestamp]] = paste0(substr(data[[timestamp]],1,4), "-", as.numeric(substr(data[[timestamp]],7,7))*3, "-01")
    data[[timestamp]] = as.Date(data[[timestamp]], format = "%Y-%m-%d")
    currentTime = paste0(substr(currentTime,1,4), "-", as.numeric(substr(currentTime,7,7))*3, "-01")
    currentTime<-as.Date(currentTime, format = "%Y-%m-%d")
  }
  else if (timestampFormat == "year") {
    data[[timestamp]] = paste0(data[[timestamp]], "-01-01")
    data[[timestamp]] = as.Date(data[[timestamp]], format = "%Y-%m-%d")
    currentTime = paste0(currentTime, "-01-01")
    currentTime = as.Date(currentTime, format = "%Y-%m-%d")
  }

  ### Select only Data in the past of currentTime
  data = data[data[[timestamp]] <= currentTime, ]
  data = data[order(data[[timestamp]]),]
  ### Determine last timestamp when thresholdValue was overtaken

  last_change<-setDT(data)[,list(no_change_since = get(timestamp)[length(get(timestamp))- last(rle(get(..value))$length) + 1],
                                 value = last(rle(get(..value))$value)),
                           by= eval(get("group"))]


  ### Determine date since when the data at least should be constant

  startdate = as.Date(currentTime) - thresholdTime
  result = last_change[last_change$no_change_since <= startdate, ]

  ### Subset data for condition thresholdTime and determine timespan in aggregation of timestampFormat
  if(timestampFormat == "day"){
    result = last_change[last_change$no_change_since <= as.Date(currentTime) - thresholdTime, ]
  }
  else if(timestampFormat =="week"){
    result = last_change[last_change$no_change_since <= as.Date(currentTime) - as.difftime(thresholdTime, units="weeks"), ]
    if(nrow(result)>0){
      result$no_change_since = ISOweek(result$no_change_since)
    }
  }
  else if(timestampFormat == "month"){
    result = last_change[last_change$no_change_since <= seq(as.Date(currentTime), length = 2, by = paste0("-",thresholdTime," months"))[2], ]
    if(nrow(result)>0){
      result$no_change_since = format(result$no_change_since, "%Y-%m")
    }
  }
  else if(timestampFormat == "quarter"){
    result = last_change[last_change$no_change_since <= seq(as.Date(currentTime), length = 2, by = paste0("-",thresholdTime," quarters"))[2], ]
    if(nrow(result)>0){
      result$no_change_since = paste0(year(result$no_change_since), "-Q", quarter(result$no_change_since))
    }
  }
  else if(timestampFormat == "year"){
    result = last_change[last_change$no_change_since <= seq(as.Date(currentTime), length = 2, by = paste0("-",thresholdTime," years"))[2], ]
    if(nrow(result)>0){
      result$no_change_since = year(result$no_change_since)
    }
  }

  result = result[order(result$no_change_since),]


  names(result) <- c(group, "no_change_since", value)


  return(result)
}


