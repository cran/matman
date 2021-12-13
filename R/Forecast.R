#' Class \code{Forecast}
#'
#' This S4 class represents the result of forecast using function \code{predictValue}.
#'
#'
#' @name Forecast-class
#' @docType class
#' @slot data (data.frame) Data frame including the predicted data and optionally the training data.
#' @slot models (list) List of fitted ARIMA models.
#' @slot value (character) Name of the value column.
#' @slot item (character) Name of the item column.
#' @slot items (character) IDs or Names of the items.
#' @section Objects from the Class: Objects can be created by calling the function
#' \code{predictValue}. This S4 class represents the result of a forecast.
#' @author Leon Binder \email{leon.binder@@th-deg.de}
#' @author Bernhard Bauer \email{bernhard.bauer@@th-deg.de}
#' @author Michael Scholz \email{michael.scholz@@th-deg.de}
#' @keywords classes
#' @examples
#' data("Amount")
#' prediction = predictValue(data = Amount,
#'     value = "amount",
#'     item = "item",
#'     timestamp = "date",
#'     temporalAggregation = "week",
#'     timeUnitsAhead = 3)
#' prediction
#' @export
setClass(
  "Forecast",
  representation(
    data = "data.frame",
    models = "list",
    value = "character",
    item = "character",
    items = "character"
  )
)

# Adds n days to a given date.
#
# n Number of days that will be added.
# date Date to which n days will be added.
.add.days = function(n, date) {
  newDate = seq(date, by = paste (n, "days"), length = 2)[2]
  return(newDate)
}

# Adds n weeks to a given date.
#
# n Number of weeks that will be added.
# date Date to which n weeks will be added.
.add.weeks = function(n, date) {
  newDate = seq(date, by = paste (n, "weeks"), length = 2)[2]
  return(newDate)
}

# Adds n months to a given date.
#
# n Number of months that will be added.
# date Date to which n months will be added.
.add.months = function(n, date) {
  date<-as.Date(paste0(substr(date,1,7),"-01"))
  newDate = seq(date, by = paste (n, "months"), length = 2)[2]
  return(newDate)
}

# Adds n quarters to a given date.
#
# n Number of quarters that will be added.
# date Date to which n quarters will be added.
.add.quarters = function(n, date) {
  date<-as.Date(paste0(substr(date,1,7),"-01"))
  newDate = seq(date, by = paste (n * 3, "months"), length = 2)[2]
  return(newDate)
}

# Adds n years to a given date.
#
# n Number of years that will be added.
# date Date to which years will be added.
.add.years = function(n, date) {
  newDate = seq(date, by = paste (n, "years"), length = 2)[2]
  return(newDate)
}

# Creates the dates for the prediction period.
#
# n Number of prediction dates.
# maxDate Last date of the historical data used for estimation.
# temporalAggregation Temporal aggregation mode (i.e., "day", "week", "month", "quarter", "year").
.getPredictionDates = function(n, maxDate, temporalAggregation = c("day", "week", "month", "quarter", "year")) {
  if (temporalAggregation == "day") {
    dates = do.call("c", lapply(1:n, .add.days, maxDate))
  } else if (temporalAggregation == "week") {
    days = do.call("c", lapply(1:n, .add.weeks, maxDate))
    dates = paste(isoyear(days), sprintf("%02d", isoweek(days)), sep = "-W")
  } else if (temporalAggregation == "month") {
    days = do.call("c", lapply(1:n, .add.months, maxDate))
    dates = paste(year(days), sprintf("%02d", month(days)), sep = "-")
  } else if (temporalAggregation == "quarter") {
    days = do.call("c", lapply(1:n, .add.quarters, maxDate))
    dates = paste(year(days), quarter(days), sep = "-Q")
  } else if (temporalAggregation == "year") {
    days = do.call("c", lapply(1:n, .add.years, maxDate))
    dates = year(days)
  }
  return(dates)
}

#' Predicts the value for items
#'
#' Predicts the value for items based on previous values. Previous values can be aggregated to value
#' per day, week, month, quarter or year. An ARIMA model is estimated for each item based on the function
#' forecast:auto.arima. The best model is selected and used for prediction. Note that only models without
#' drift term will be considered in order to ensure consistent predictions.
#'
#' @param data Data frame including previous values.
#' @param value Name of the column representing the item value.
#' @param item Name of the column representing the item ID or the item name.
#' @param timestamp Name of the column including the timestamp. This column should be in POSIX or date-format.
#' @param temporalAggregation Temporal aggregation mode (i.e., "day", "week", "month", "quarter", "year").
#' @param aggregationFun Function for aggregating the value column. Default is sum.
#' @param timeUnitsAhead Integer indicating the number of time units (i.e., days, weeks, months, quarters or years) the should be predicted.
#' @param digits Integer indicating the number of significant digits used for the predicted values.
#' @param expand Logical indicating whether the data will be expanded after they are aggregated. Default is not (FALSE).
#' @param keepPreviousData Logical indicating whether the data from the given data frame will be added to the result or not. Default is not (FALSE).
#' @param level Numeric value representing the confidence level for the predictions. The default is 0.95 (i.e. lower level = 0.025 and upper level
#' = 0.975).
#' @param ... Further arguments for function forecast::auto.arima.
#' @return Returns a \code{Forecast} object.
#' @seealso \code{\link[forecast]{auto.arima}} \code{\link[=Forecast-class]{Forecast}}
#' @author Leon Binder \email{leon.binder@@th-deg.de}
#' @author Bernhard Bauer \email{bernhard.bauer@@th-deg.de}
#' @author Michael Scholz \email{michael.scholz@@th-deg.de}
#' @examples
#' # Simple Example
#' data("Amount")
#' prediction = predictValue(data = Amount,
#'     value = "amount",
#'     item = "item",
#'     timestamp = "date",
#'     temporalAggregation = "week",
#'     timeUnitsAhead = 3)
#' prediction
#'
#' # More Sophisticated Example
#' data("Amount")
#' prediction = predictValue(data = Amount,
#'     value = "amount",
#'     item = "item",
#'     timestamp = "date",
#'     temporalAggregation = "week",
#'     aggregationFun = mean,
#'     timeUnitsAhead = 5,
#'     digits = 4,
#'     keepPreviousData = TRUE,
#'     level = 0.9,
#'     trace = TRUE)
#' prediction
#' @export
predictValue = function(data,
                         value,
                         item,
                         timestamp,
                         temporalAggregation = c("day", "week", "month", "quarter", "year"),
                         aggregationFun = sum,
                         timeUnitsAhead = 1,
                         digits = 3,
                         expand = F,
                         keepPreviousData = F,
                         level = 0.95,
                         ...) {
  if (!value %in% names(data)) {
    stop(paste("Data does not include column ", value, ".", sep = ""))
  }
  if (!all(item %in% names(data))) {
    stop(paste("Data does not include columns ", item, ".", sep = ""))
  }
  if (!timestamp %in% names(data)) {
    stop(paste("Data does not include column ", timestamp, ".", sep = ""))
  }
  if (!is.numeric(timeUnitsAhead) || timeUnitsAhead <= 0) {
    warning("timeUnitsAhead is set to 1")
    timeUnitsAhead = 1
  }
  if (!is.numeric(digits) || digits <= 0) {
    warning("digits is set to 3")
    digits = 3
  }
  if (!is.numeric(level) || level <= 0 || level >= 1) {
    warning("level is set to 0.95")
    level = 0.95
  }
  aggregatedData = as.data.frame(aggregateData(data = data,
                                           value = value,
                                           item = item,
                                           timestamp = timestamp,
                                           temporalAggregation = temporalAggregation,
                                           aggregationFun = aggregationFun))
  if (expand) {
    aggregatedData = expandData(data = aggregatedData,
                                expand = item,
                                expandTo = "all",
                                valueColumns = value,
                                valueLevels = 0,
                                timestamp = temporalAggregation,
                                timestampFormat = temporalAggregation)
  }
  items = unique(data[[item]])
  maxDate = max(data[[timestamp]])
  predictionDates = .getPredictionDates(timeUnitsAhead, maxDate, temporalAggregation)
  predictions = vector()
  models = list()
  for(i in 1:length(items)) {
    itemData = aggregatedData[aggregatedData[[item]] == items[i], ]
    model = auto.arima(itemData[[value]], allowdrift = F, ... = ...)
    prediction = predict(model, n.ahead = timeUnitsAhead, se.fit = T)
    est = round(as.numeric(prediction$pred), digits)
    alpha = (1-level)/2
    lower = round(as.numeric(prediction$pre) + qnorm(alpha) * as.numeric(prediction$se), digits)
    lower = ifelse(lower < 0, 0, lower)
    upper = round(as.numeric(prediction$pre) + qnorm(1-alpha) * as.numeric(prediction$se), digits)
    predictions = rbind(predictions, cbind(items[i], predictionDates, est, lower, upper))
    models[[i]] = model
  }
  predictions = as.data.frame(predictions)
  names(predictions) = c(item, temporalAggregation, value, "lower", "upper")
  if (keepPreviousData) {
    aggregatedData$lower = aggregatedData[[value]]
    aggregatedData$upper = aggregatedData[[value]]
    predictions = rbind(aggregatedData, predictions)
    predictions = predictions[order(predictions$item, predictions[[temporalAggregation]]),]
  }
  forecast = new("Forecast", data = predictions, models = models, value = value, item = item, items = as.character(items))
  return(forecast)
}

#' Shows a Forecast object
#'
#' Shows the predicted data of a Forecast object. If the Forecast object was created using keepPreviousData = TRUE, also the training data
#' are shown
#'
#' @param object The \code{Forecast} object
#' @author Leon Binder \email{leon.binder@@th-deg.de}
#' @author Bernhard Bauer \email{bernhard.bauer@@th-deg.de}
#' @author Michael Scholz \email{michael.scholz@@th-deg.de}
#' @seealso \code{\link[=Forecast-class]{Forecast}}
#' @keywords methods
#' @docType methods
#' @examples
#' data("Amount")
#' prediction = predictValue(data = Amount,
#'     value = "amount",
#'     item = "item",
#'     timestamp = "date",
#'     temporalAggregation = "week",
#'     timeUnitsAhead = 3)
#' prediction
setMethod("show", "Forecast", function(object) {
  print(object@data)
})

#' Prints the summary of a Forecast object
#'
#' Summarizes the fitted models estimated for predicting item values (e.g., demand, stock).
#'
#' @param object Object of class Forecast
#' @return A data frame showing a summary of fitted models.
#' @author Leon Binder \email{leon.binder@@th-deg.de}
#' @author Bernhard Bauer \email{bernhard.bauer@@th-deg.de}
#' @author Michael Scholz \email{michael.scholz@@th-deg.de}
#' @seealso \code{\link{predictValue}} \code{\link[=Forecast-class]{Forecast}}
#' @aliases summary,Forecast-method
#' @keywords methods
#' @docType methods
#' @examples
#' data("Amount")
#' prediction = predictValue(data = Amount,
#'     value = "amount",
#'     item = "item",
#'     timestamp = "date",
#'     temporalAggregation = "week",
#'     timeUnitsAhead = 3)
#' summary(prediction)
#' @export
setMethod("summary", "Forecast", function(object) {
  model = unlist(lapply(object@models, FUN = function(y) paste("ARIMA(", paste(y$arma[c(1, 6, 2)], collapse = ","), ")", sep = "")))
  item = object@items
  logLik = unlist(lapply(object@models, FUN = function(y) y$loglik))
  aic = unlist(lapply(object@models, FUN = function(y) y$aic))
  bic = unlist(lapply(object@models, FUN = function(y) y$bic))
  summary = data.frame(item = item, model = model, logLik = logLik, AIC = aic, BIC = bic)
  return(summary)
})
