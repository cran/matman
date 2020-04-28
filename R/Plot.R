.movingAverage <- function(x, n = 5, type = c("s", "w")) {
  # See pracma.
  # movavg(1:10, n=5, type="s")
  y <- numeric(length(x))
  if (type == "s") {
    for (k in 1:(n - 1)) y[k] <- mean(x[1:k])
    for (k in n:length(x)) y[k] <- mean(x[(k - n + 1):k])
  }
  else if (type == "w") {
    for (k in 1:(n - 1)) y[k] <- 2 * sum((k:1) * x[k:1]) / (k * (k + 1))
    for (k in n:length(x)) y[k] <- 2 * sum((n:1) * x[k:(k - n + 1)]) / (n * (n + 1))
  }
  return(y)
}


#' Plots the development of the values
#'
#' Plots a bar chart that shows the sum of the value column for a certain time interval.
#'
#' @param data                 Data frame or matrix on which the ABC analysis is performed.
#' @param item                 Name of the column including the item name or identifier (e.g., product name, EAN).
#' @param item_id              Name of the item that will be displayed.
#' @param value                Name of the column variable that contains the values.
#' @param timestamp            Name of the column including the timestamp. This column should be in POSIX or date-format.
#' @param temporalAggregation  Temporal aggregation for the XYZ-analysis (i.e., "day", "week", "month", "quarter", "year").
#' @param expand               Indicator if the data should be expanded with time intervals that have no data.
#' @param withTrendLine        Indicator if a trend line should be displayed in the bar chart.
#' @param windowLength         Backwards window length.
#' @param trendLineType        If "s" the simple and if "w" the weighted moving average is calculated.
#' @return                     A plotly bar chart, that shows the development of the value column.
#'
#' @author Leon Binder \email{leon.binder@@th-deg.de}
#' @author Bernhard Bauer \email{bernhard.bauer@@th-deg.de}
#' @author Michael Scholz \email{michael.scholz@@th-deg.de}
#' @keywords methods
#' @docType methods
#' @examples
#' data("Amount")
#' plotValueSeries(Amount,
#'                 item = "item",
#'                 item_id = "45186",
#'                 value = "amount",
#'                 timestamp = "date",
#'                 temporalAggregation = "week",
#'                 withTrendLine = TRUE,
#'                 windowLength = 10,
#'                 trendLineType = "w")
#' @export
plotValueSeries <- function(data,
                            item,
                            item_id,
                            value,
                            timestamp,
                            temporalAggregation = c("day", "week", "month", "quarter", "year"),
                            expand = TRUE,
                            withTrendLine = TRUE,
                            windowLength = 5,
                            trendLineType = "s") {

  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Package \"plotly\" needed for this function to work. Please install it.")
  } else {
    if (!item %in% names(data)) {
      stop(paste("Data does not include column ", item, ".", sep = ""))
    }
    if (!item_id %in% data[[item]]) {
      stop(paste("Item column does not include the id ", item_id, ".", sep = ""))
    }
    if (!all(value %in% names(data))) {
      stop(paste("Data does not include columns ", value, ".", sep = ""))
    }
    if (!timestamp %in% names(data)) {
      stop(paste("Data does not include column ", timestamp, ".", sep = ""))
    }

    selected = data[data[[item]] == item_id,]
    selected = selected[!is.na(selected[[value]]),]

    aggregated = aggregateData(
      data = selected,
      item = item,
      value = value,
      timestamp = timestamp,
      temporalAggregation = temporalAggregation
    )

    if (expand) {
      expanded = expandData(
        data = aggregated,
        expand = item,
        expandTo = "all",
        valueColumns = c(value),
        valueLevels = c(0),
        timestamp = temporalAggregation,
        timestampFormat = temporalAggregation,
        keepData = TRUE
      )
    } else {
      expanded = aggregated
    }

    p = plotly::plot_ly(x = expanded[[temporalAggregation]], y = expanded[[value]], type = "bar", name="") %>%
      plotly::layout(title = paste(value, " - ", item, ": ", item_id, sep=""),
             xaxis = list(title = timestamp),
             yaxis = list(title = value),
             showlegend = FALSE)

    if (withTrendLine) {
      #model = eval(parse(text = paste("lm(", value, " ~ ", temporalAggregation, ", data = expanded)")))
      #p = p %>% plotly::add_trace(data = expanded, x = expanded[[temporalAggregation]], y = fitted(model),
      #                            type = "scatter", mode = "lines")

      y = .movingAverage(x = expanded[[value]], n = windowLength, type = trendLineType)
      p = p %>% plotly::add_trace(data = expanded, x = expanded[[temporalAggregation]], y = y,
                                  type = "scatter", mode = "lines", name="")
    }

    return(p)
  }

}

