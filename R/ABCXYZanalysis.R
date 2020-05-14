#' Class \code{ABCXYZData}
#'
#' This S4 class represents the result of an ABC/XYZ analysis.
#'
#'
#' @name ABCXYZData-class
#' @docType class
#' @slot data (data.frame) The result table of an ABC/XYZ analysis.
#' @slot type (character) The type of the analysis that has been performed. This is either 'abc' or 'abcxyz'.
#' @slot value (character) The name of the value column in the result table.
#' @slot item (character) Vector of the names of the item columns in the result table.
#' @section Objects from the Class: Objects can be created by calling the function
#' \code{computeABCXYZ}. This S4 class represents the result of an ABC/XYZ analysis.
#' @author Leon Binder \email{leon.binder@@th-deg.de}
#' @author Bernhard Bauer \email{bernhard.bauer@@th-deg.de}
#' @author Michael Scholz \email{michael.scholz@@th-deg.de}
#' @keywords classes
#' @examples
#' data("Amount")
#' abcResult = computeABCXYZAnalysis(data = Amount,
#'     value = "value",
#'     item = "item",
#'     timestamp = "date")
#' abcResult
#' @export
setClass(
  "ABCXYZData",
  representation(
    data = "data.frame",
    type = "character",
    value = "character",
    item = "character"
  )
)

#' Class \code{ABCXYZComparison}
#'
#' This S4 class represents the result of a comparison of two ABC/XYZ analysis results.
#'
#'
#' @name ABCXYZComparison-class
#' @docType class
#' @section Objects from the Class: Objects can be created by calling the function
#' \code{compare} function. This S4 class represents the result of a comparison of two
#' ABC/XYZ analysis results.
#' @slot data (data.frame) The comparison result as data.frame.
#' @slot type (character) The type of the analysis that has been performed. This is either 'abc' or 'abcxyz'.
#' @slot valueDiff (numeric) The difference between the value of an item in ABC/XYZ analysis A and the value of the same item
#' in ABC/XYZ analysis B that is required to consider the item in the comparison.
#' @slot xyzCoefficientDiff (numeric) The difference between the xyz coefficient of an item in ABC/XYZ analysis A and the
#' xyz coefficient of the same item in ABC/XYZ analysis B that is required to consider the item in the comparison.
#' @slot unequalABC (logical) If \code{TRUE} only items are returned, where the ABC-Classes are different. If FALSE only
#' items are returned, where the ABC-Classes are equal. If \code{NA}, no further restriction takes place based on the column ABC.
#' @slot unequalXYZ (logical) If \code{TRUE} only items are returned, where the XYZ-Classes are different. If FALSE only
#' items are returned, where the XYZ-Classes are equal. If \code{NA}, no further restriction takes place based on the column XYZ.
#' @author Leon Binder \email{leon.binder@@th-deg.de}
#' @author Bernhard Bauer \email{bernhard.bauer@@th-deg.de}
#' @author Michael Scholz \email{michael.scholz@@th-deg.de}
#' @keywords classes
#' @examples
#' data("Amount")
#' data1 = Amount[sample(1:nrow(Amount), 1000),]
#' data2 = Amount[sample(1:nrow(Amount), 1000),]
#' abcxyzData1 = computeABCXYZAnalysis(data1, value = "value", item = "item", timestamp = "date")
#' abcxyzData2 = computeABCXYZAnalysis(data2, value = "value", item = "item", timestamp = "date")
#' comparison = compare(abcxyzData1, abcxyzData2)
#' comparison
#' @export
setClass(
  "ABCXYZComparison",
  representation(
    data = "data.frame",
    type = "character",
    valueDiff = "numeric",
    xyzCoefficientDiff = "numeric",
    unequalABC = "logical",
    unequalXYZ = "logical"
  )
)


# Computes an ABC-Analysis.
#
# aggregatedData  A data frame with one row per item and the column Sum.
# AB              Threshold (in percent) between category A and B.
# BC              Threshold (in percent) between category B and C.
#
# Returns a copy of the original data frame with the columns Percentage, CumulativePercentage and ABC.
.computeABCAnalysis = function(aggregatedData, value, AB, BC) {
  # Calculation of the cumulative percentage.
  aggregatedData$percentage = aggregatedData[[value]] / sum(aggregatedData[[value]]) * 100
  aggregatedData = aggregatedData[order(aggregatedData$percentage, decreasing = TRUE),]
  aggregatedData$cumulative.percentage = cumsum(aggregatedData$percentage)

  # Assignment of the classes A, B or C to the items.
  aggregatedData$abc[aggregatedData$cumulative.percentage <= AB] = "A"
  aggregatedData$abc[aggregatedData$cumulative.percentage > AB &
                       aggregatedData$cumulative.percentage <= BC] = "B"
  aggregatedData$abc[aggregatedData$cumulative.percentage > BC] = "C"

  return(aggregatedData)

}


# Standard deviation for the whole population.
#
# x           Value vector
# timePeriods Number of total time periods for filling zeros. No zeros will be filled if timePeriods is NA.
#
# Returns the standard deviation.
.sd.g = function(x, timePeriods = NA) {
  if (!is.na(timePeriods) && timePeriods > 0) {
    x = c(x, rep(0, timePeriods - length(x)))
  }
  x = x / mean(x)
  sdg = sqrt(sum((x - mean(x))^2) / (length(x)))
  return(sdg)
}


# Computes a XYZ-Analyse.
#
# data                 Data frame or matrix on which the ABC analysis is performed.
# value                Name of the column variable that contains the value for the ABC and XYZ analysis.
# item                 Names of the columns including the item names or identifiers (e.g., product name, EAN).
# timestamp            Name of the column including the timestamp. This column should be in POSIX or Date-format.
# temporalAggregation  Temporal aggregation mode for the XYZ-analysis.
# XY                   Threshold (in percent) between category X and Y.
# YZ                   Threshold (in percent) between category Y and Z.
# ignoreZeros          Whether zero values should be ignored in XYZ-analysis.
#
# Returns a copy of the original data frame with the columns XYZCoefficient and XYZ.
.computeXYZAnalysis = function(data, value, item, timestamp, temporalAggregation, XY, YZ, ignoreZeros) {

  aggregatedData = aggregateData(data, value, item, timestamp, temporalAggregation)

  if (ignoreZeros) {
    # Time periods (days, weeks, months, quarters, years) with no values are ignored.
    # The XYZ-Coefficient is only computed on the time periods that have valid values.
    nonZeros = aggregatedData[[value]] != 0
    aggregatedData = aggregatedData[nonZeros,]

    xyzData = as.data.table(aggregatedData)[,list(xyz.coefficient = .sd.g(get(..value))), keyby = eval(get("item"))]
  } else {
    # Time periods (days, weeks, months, quarters, years) with no values are considered.
    # First the number of all time periods e. g. days between the min and max date are computed.
    #if (temporalAggregation == "day") {
    #  numberTimePeriods = difftime(max(data[[timestamp]], na.rm=TRUE),
    #                               min(data[[timestamp]], na.rm=TRUE), units="days")
    #} else if (temporalAggregation == "week") {
    #  numberTimePeriods = difftime(max(data[[timestamp]], na.rm=TRUE),
    #                                                   min(data[[timestamp]], na.rm=TRUE), units="weeks")
    #} else if (temporalAggregation == "month") {
    #  numberTimePeriods = 12*(year(max(data[[timestamp]], na.rm=TRUE))-year(min(data[[timestamp]], na.rm=TRUE)))+
    #    (month(max(data[[timestamp]], na.rm=TRUE))-month(min(data[[timestamp]], na.rm=TRUE)))+1
    #} else if (temporalAggregation == "quarter") {
    #  numberTimePeriods = difftime(max(data[[timestamp]], na.rm=TRUE),
    #                               min(data[[timestamp]], na.rm=TRUE), units="weeks") / 52.25 * 4
    #} else if (temporalAggregation == "year") {
    #  numberTimePeriods = floor(as.numeric(difftime(max(data[[timestamp]], na.rm = TRUE),
    #                                min(data[[timestamp]], na.rm = TRUE), units = "days") / 365.25))
    #}

    # Second the number of all time periods is ceiled, because the value is not allowed to be
    # smaller than the amount of rows in aggregatedData.
    #numberTimePeriods = as.numeric(ceiling(numberTimePeriods))

    daily<-data.frame(date = seq(as.Date(min(data[[timestamp]])), as.Date(max(data[[timestamp]])), by="days"))

    if (temporalAggregation == "day") {
      numberTimePeriods = length(unique(daily$date))
    } else if (temporalAggregation == "week") {
      weeks <- unique(ISOweek(daily$date))
      numberTimePeriods = length(weeks)
    } else if (temporalAggregation == "month") {
      months <- unique(paste(year(daily[["date"]]), sprintf("%02d", month(daily[["date"]])), sep = "-"))
      numberTimePeriods = length(months)
    } else if (temporalAggregation == "quarter") {
      quaters <- unique(paste(year(daily[["date"]]), quarter(daily[["date"]]), sep = "-Q"))
      numberTimePeriods = length(quaters)
    } else if (temporalAggregation == "year") {
      years <- unique(year(daily[["date"]]))
      numberTimePeriods = length(years)
    }

    # Third the XYZ-Coefficient is calculated. For all missing time periods the value 0 is considered.
    xyzData = as.data.table(aggregatedData)[,list(xyz.coefficient = .sd.g(get(..value), timePeriods = numberTimePeriods)), keyby = eval(get("item"))]
  }

  # Assignment of the classes X, Y, Z.
  xyzData$xyz = ""
  xyzData$xyz[xyzData$xyz.coefficient <= XY] = "X"
  xyzData$xyz[xyzData$xyz.coefficient > XY & xyzData$xyz.coefficient <= YZ] = "Y"
  xyzData$xyz[xyzData$xyz.coefficient > YZ] = "Z"

  return(xyzData)
}



#' Performs an ABC/XYZ analysis
#'
#' Divides a given data frame into 3 classes, A, B, C, according to the value of one column (e.g., revenue).
#'
#' @param data                 Data frame or matrix on which the ABC analysis is performed.
#' @param value                Name of the column variable that contains the value for the ABC and XYZ analysis.
#' @param item                 Names of the columns including the item names or identifiers (e.g., product name, EAN).
#' @param timestamp            Name of the column including the timestamp. This column should be in POSIX or date-format.
#' @param temporalAggregation  Temporal aggregation for the XYZ-analysis (i.e., "day", "week", "month", "quarter", "year").
#' @param AB                   Threshold (in percent) between category A and B.
#' @param BC                   Threshold (in percent) between category B and C.
#' @param XY                   Threshold (in percent) between category X and Y.
#' @param YZ                   Threshold (in percent) between category Y and Z.
#' @param ignoreZeros          Whether zero values should be ignored in XYZ-analysis.
#' @return Returns an \code{ABCXYZData} object.
#' @author Leon Binder \email{leon.binder@@th-deg.de}
#' @author Bernhard Bauer \email{bernhard.bauer@@th-deg.de}
#' @author Michael Scholz \email{michael.scholz@@th-deg.de}
#' @seealso \code{\link[=ABCXYZData-class]{ABCXYZData}} \code{\link[=ABCXYZData-class]{summary}}
#'
#' @examples
#' # ABC Analysis
#' data("Amount")
#' abcResult = computeABCXYZAnalysis(data = Amount,
#'     value = "value",
#'     item = "item",
#'     timestamp = "date")
#'
#' # ABC/XYZ Analysis
#' data("Amount")
#' abcxyzResult = computeABCXYZAnalysis(data = Amount,
#'     value = "value",
#'     item = "item",
#'     timestamp = "date",
#'     temporalAggregation = "week",
#'     XY = 0.3, YZ = 0.5)
#' @export
computeABCXYZAnalysis = function(data,
                                 value,
                                 item,
                                 timestamp,
                                 temporalAggregation = c("day", "week", "month", "quarter", "year"),
                                 AB = 80,
                                 BC = 95,
                                 XY = NA,
                                 YZ = NA,
                                 ignoreZeros = FALSE) {
  if (AB >= BC) {
    stop("Threshold AB should be lower than threshold BC.")
  }
  if(AB < 0 || BC < 0 || AB > 100 || BC > 100){
    stop("Values of AB and BC have to be in [0,100]")
  }
  if (!is.na(XY) && !is.na(YZ) && XY >= YZ) {
    stop("Threshold XY should be lower than threshold YZ.")
  }
  if (!is.na(XY) && !is.na(YZ) && (XY < 0 || YZ < 0)) {
    stop("XY and YZ need values > 0")
  }

  if (!value %in% names(data)) {
    stop(paste("Data does not include column ", value, ".", sep = ""))
  }
  if (!all(item %in% names(data))) {
    stop(paste("Data does not include columns ", item, ".", sep = ""))
  }
  if (!timestamp %in% names(data)) {
    stop(paste("Data does not include column ", timestamp, ".", sep = ""))
  }
  # Aggregation for ABC-Analysis.
  dataForABC = aggregateData(data, value, item, timestamp, temporalAggregation = "total")

  # ABC-Analysis.
  abcData = .computeABCAnalysis(dataForABC, value, AB, BC)
  colnames(abcData)[which(colnames(abcData) == "sum")] = paste(value, "sum", sep = "")

  if (all(is.na(XY), is.na(YZ))) {
    abcData$class = abcData$abc
    abcxyzResult = new("ABCXYZData", data = abcData, type = "abc", value = value, item = item)
  } else {

    # XYZ-Analysis.
    xyzData = .computeXYZAnalysis(data,
                                  value,
                                  item,
                                  timestamp,
                                  temporalAggregation,
                                  XY,
                                  YZ,
                                  ignoreZeros)

    # ABC/XYZ-Analysis.
    abcxyzData = inner_join(abcData, xyzData, by = item)
    abcxyzData$class = paste(abcxyzData$abc, abcxyzData$xyz, sep = "")

    abcxyzResult = new("ABCXYZData", data = abcxyzData, type = "abcxyz", value = value, item = item)
  }
  return(abcxyzResult)
}


#' Shows an ABCXYZData object
#'
#' Shows the \code{ABCXYZData} object as a table consisting of the absolute and relative amount of each item,
#' the cumulative relative amount and the ABC-class. If XY and YZ parameters have been specified for
#' computing the ABCXYZData object, the table also includes a column for the XYZ coefficient, the XYZ-
#' class and the ABC/XYZ-class.
#'
#' @param object The \code{ABCXYZData} object
#' @author Leon Binder \email{leon.binder@@th-deg.de}
#' @author Bernhard Bauer \email{bernhard.bauer@@th-deg.de}
#' @author Michael Scholz \email{michael.scholz@@th-deg.de}
#' @seealso \code{\link[=ABCXYZData-class]{ABCXYZData}} \code{\link{computeABCXYZAnalysis}}
#' @keywords methods
#' @docType methods
#' @examples
#' data("Amount")
#' abcResult = computeABCXYZAnalysis(data = Amount,
#'     value = "value",
#'     item = "item",
#'     timestamp = "date")
#' abcResult
setMethod("show", "ABCXYZData", function(object) {
  print(object@data)
})


#' Summarizes an S4 object
#'
#' Summarizes an S4 object.
#'
#' @param object S4 object.
#' @param ... Optional parameters.
#' @return Summary.
#' @author Leon Binder \email{leon.binder@@th-deg.de}
#' @author Bernhard Bauer \email{bernhard.bauer@@th-deg.de}
#' @author Michael Scholz \email{michael.scholz@@th-deg.de}
#' @seealso \code{\link[=ABCXYZData-class]{summary}} \code{\link[=ABCXYZComparison-class]{summary}} \code{\link[=Forecast-class]{summary}}
#' @aliases summary
#' @keywords methods
#' @docType methods
#' @examples
#' data("Amount")
#' abcResult = computeABCXYZAnalysis(data = Amount,
#'     value = "value",
#'     item = "item",
#'     timestamp = "date")
#' summary(abcResult)
#'
#' @export
setGeneric("summary", function(object, ...)
  standardGeneric("summary"))


#' Prints the result summary of an ABC/XYZ analysis
#'
#' Summarizes the items count and value sum grouped by the different ABC- or ABC/XYZ-Classes.
#'
#' @param object Object of class \code{ABCXYZData}.
#' @param withMissing Logical indicating whether missing categories will be shown. Default is \code{FALSE}.
#' @return A \code{data.table} with the summarized results.
#' @author Leon Binder \email{leon.binder@@th-deg.de}
#' @author Bernhard Bauer \email{bernhard.bauer@@th-deg.de}
#' @author Michael Scholz \email{michael.scholz@@th-deg.de}
#' @seealso \code{\link{computeABCXYZAnalysis}} \code{\link[=ABCXYZData-class]{ABCXYZData}}
#' @aliases summary,ABCXYZData-method
#' @keywords methods
#' @docType methods
#' @examples
#' # ABC Analysis
#' data("Amount")
#' abcResult = computeABCXYZAnalysis(data = Amount,
#'     value = "value",
#'     item = "item",
#'     timestamp = "date")
#' summary(abcResult)
#'
#' # ABC/XYZ Analysis
#' data("Amount")
#' abcxyzResult = computeABCXYZAnalysis(data = Amount,
#'     value = "value",
#'     item = "item",
#'     timestamp = "date",
#'     temporalAggregation = "week",
#'     XY = 0.3, YZ = 0.5)
#' summary(abcxyzResult)
#' @export
setMethod("summary", "ABCXYZData", function(object, withMissing = FALSE) {
  temp = as.data.table(object@data)
  summary = temp %>%
    group_by(.dots = "class") %>%
    summarize(items = n(), sum = sum(get(object@value), na.rm = TRUE)) %>%
    as.data.table

  if (object@type == "abc") {
    classes = c("A", "B", "C")
  } else {
    classes = c("AX", "AY", "AZ", "BX", "BY", "BZ", "CX", "CY", "CZ")
  }

  if (withMissing) {
    missingClasses = classes[!classes %in% summary$class]
    summary = rbind(summary,
                    data.table("class" = missingClasses,
                               "items" = rep(0, length(missingClasses)),
                               "sum" = rep(0, length(missingClasses))))
  }

  summary$percentage.items = paste(round(summary$items / sum(summary$items) * 100, 2), "%")
  summary$percentage.sum = paste(round(summary$sum / sum(summary$sum) * 100, 2), "%")
  summary = summary[order(summary$class),]
  return(summary)
})


#' Compares two S4 objects
#'
#' Compares two S4 objects.
#'
#' @param object1      First S4 object.
#' @param object2      Second S4 object.
#' @param ...          Further comparison parameters.
#' @return Comparison result.
#' @author Leon Binder \email{leon.binder@@th-deg.de}
#' @author Bernhard Bauer \email{bernhard.bauer@@th-deg.de}
#' @author Michael Scholz \email{michael.scholz@@th-deg.de}
#' @seealso \code{\link[=ABCXYZComparison-class]{compare}}
#' @aliases compare
#' @keywords methods
#' @docType methods
#' @examples
#' data("Amount")
#' data1 = Amount[sample(1:nrow(Amount), 1000),]
#' data2 = Amount[sample(1:nrow(Amount), 1000),]
#' abcxyzData1 = computeABCXYZAnalysis(data1, value = "value", item = "item", timestamp = "date")
#' abcxyzData2 = computeABCXYZAnalysis(data2, value = "value", item = "item", timestamp = "date")
#' comparison = compare(abcxyzData1, abcxyzData2)
#' @export
setGeneric("compare", function(object1, object2, ...)
  standardGeneric("compare"))


#' Compares the results of two ABC/XYZ analyses
#'
#' Compares the class assignments of two ABC- or two ABC/XYZ analyses.
#'
#' @param object1      Object of class \code{ABCXYZData}.
#' @param object2      Object of class \code{ABCXYZData}.
#' @param valueDiff    Only items with a difference of the column value larger than valueDiff between the
#'                     first and second ABC-XYZ-Analysis are returned. In the comparison data.frame a new
#'                     column is added for the difference in the value columns.
#' @param xyzCoefficientDiff Only items with a difference of the column xyzCoefficient larger than the
#'                           xyzCoefficientDiff between the first and second ABC-XYZ-Analysis are returned.
#'                           In the comparison data.frame a new column is added for the difference in the
#'                           xyzCoefficient columns.
#' @param unequalABC   If \code{TRUE} only items are returned, where the ABC-Classes are different. If \code{FALSE} only
#'                     items are returned, where the ABC-Classes are equal. If \code{NA}, no further restriction
#'                     takes place based on the column ABC.
#' @param unequalXYZ   If \code{TRUE} only items are returned, where the XYZ-Classes are different. If \code{FALSE} only
#'                     items are returned, where the XYZ-Classes are equal. If \code{NA}, no further restriction
#'                     takes place based on the column XYZ.
#' @return An \code{ABCYXZComparison} object.
#' @author Leon Binder \email{leon.binder@@th-deg.de}
#' @author Bernhard Bauer \email{bernhard.bauer@@th-deg.de}
#' @author Michael Scholz \email{michael.scholz@@th-deg.de}
#' @seealso \code{\link[=ABCXYZComparison-class]{ABCXYZComparison}}
#' @aliases compare,ABCXYZData-method
#' @keywords methods
#' @docType methods
#' @examples
#' data("Amount")
#' data1 = Amount[sample(1:nrow(Amount), 1000),]
#' data2 = Amount[sample(1:nrow(Amount), 1000),]
#' abcxyzData1 = computeABCXYZAnalysis(data1, value = "value", item = "item", timestamp = "date")
#' abcxyzData2 = computeABCXYZAnalysis(data2, value = "value", item = "item", timestamp = "date")
#' comparison = compare(abcxyzData1, abcxyzData2)
#' @export
setMethod("compare", signature = c("ABCXYZData", "ABCXYZData"), function(object1,
                                            object2,
                                            valueDiff = NA,
                                            xyzCoefficientDiff = NA,
                                            unequalABC = NA,
                                            unequalXYZ = NA) {
  abcData1 = object1@data
  abcData2 = object2@data

  if (object1@type != object2@type) {
    stop("The ABCXYZData are not comparable because they are of different types.")
  }

  if (object1@type == "abc" && object2@type == "abc") {
    type = "abc"
  } else if (object1@type == "abcxyz" && object2@type == "abcxyz") {
    type = "abcxyz"
  }

  comparison = full_join(abcData1,
                         abcData2,
                         by = setNames(object1@item, object2@item),
                         suffix = c(".1", ".2"))

  # Keep all comparisons where the value column changed by some margin.
  if (!is.na(valueDiff)) {
    comparison$value.diff = abs(comparison[[paste0(object1@value, ".2")]] - comparison[[paste0(object2@value, ".1")]])
    comparison = comparison[which(comparison$value.diff > valueDiff),]
  }

  # Keep all comparisons where the XYZ-Coefficient changed by some margin.
  if (!is.na(xyzCoefficientDiff) && object1@type == "abcxyz" && object2@type == "abcxyz") {
    comparison$xyz.coefficient.diff = abs(comparison$xyz.coefficient.1 - comparison$xyz.coefficient.2)
    comparison = comparison[which(comparison$xyz.coefficient.diff > xyzCoefficientDiff),]
  }

  # How big were the class changes? Positiv for changes to upper classes, Negativ for changes to lower classes.
  comparison$abc.comparison = sapply(comparison$abc.1, "utf8ToInt") - sapply(comparison$abc.2, "utf8ToInt")
  if (object1@type == "abcxyz" && object2@type == "abcxyz") {
    comparison$xyz.comparison = sapply(comparison$xyz.1, "utf8ToInt") - sapply(comparison$xyz.2, "utf8ToInt")
  }

  # Only where ABC is equal or not equal to ABC.
  if (!is.na(unequalABC)) {
    if (unequalABC) {
      comparison = comparison[which(comparison$abc.comparison != 0),]
    } else {
      comparison = comparison[which(comparison$abc.comparison == 0),]
    }
  }

  # Only where XYZ is equal or not equal to XYZ.
  if (!is.na(unequalXYZ) && object1@type == "abcxyz" && object2@type == "abcxyz") {
    if (unequalXYZ) {
      comparison = comparison[which(comparison$xyz.comparison != 0),]
    } else {
      comparison = comparison[which(comparison$xyz.comparison == 0),]
    }
  }

  comparisonResult = new("ABCXYZComparison",
                         data = comparison,
                         type = type,
                         valueDiff = as.numeric(valueDiff),
                         xyzCoefficientDiff = as.numeric(xyzCoefficientDiff),
                         unequalABC = unequalABC,
                         unequalXYZ = unequalXYZ)
  return(comparisonResult)
})


#' Shows an ABCXYZComparison object
#'
#' Shows an \code{ABCXYZComparison} object as a table consisting of the absolute and relative amount of each item,
#' the cumulative relative amount and the ABC-class for both \code{ABCXYZData} objects. It furthermore shows the
#' ABC comparison of the two objects. If XY and YZ parameters have been specified for computing the
#' \code{ABCXYZData} object, the table also includes a column for the XYZ coefficient, the XYZ-class, the
#' ABC/XYZ-class and the XYZ comparison.
#'
#' @param object The \code{ABCXYZComparison} object
#' @author Leon Binder \email{leon.binder@@th-deg.de}
#' @author Bernhard Bauer \email{bernhard.bauer@@th-deg.de}
#' @author Michael Scholz \email{michael.scholz@@th-deg.de}
#' @seealso \code{\link[=ABCXYZComparison-class]{ABCXYZComparison}} \code{\link[=ABCXYZComparison-class]{compare}}
#' @keywords methods
#' @docType methods
#' @examples
#' data("Amount")
#' data1 = Amount[sample(1:nrow(Amount), 1000),]
#' data2 = Amount[sample(1:nrow(Amount), 1000),]
#' abcxyzData1 = computeABCXYZAnalysis(data1, value = "value", item = "item", timestamp = "date")
#' abcxyzData2 = computeABCXYZAnalysis(data2, value = "value", item = "item", timestamp = "date")
#' comparison = compare(abcxyzData1, abcxyzData2)
#' comparison
setMethod("show", "ABCXYZComparison", function(object) {
  print(object@data)
})


#' Prints the summary of the comparison of two ABC/XYZ analyses
#'
#' Summarizes the differences between two \code{ABCXYZData} objects.
#'
#' @param object Object of class ABCXYZComparison.
#' @param withMissing Logical indicating whether missing categories will be shown. Default is \code{FALSE}.
#' @return A contingency table showing the differences.
#' @author Leon Binder \email{leon.binder@@th-deg.de}
#' @author Bernhard Bauer \email{bernhard.bauer@@th-deg.de}
#' @author Michael Scholz \email{michael.scholz@@th-deg.de}
#' @seealso \code{\link[=ABCXYZComparison-class]{compare}} \code{\link[=ABCXYZComparison-class]{ABCXYZComparison}}
#' @aliases summary,ABCXYZComparison-method
#' @keywords methods
#' @docType methods
#' @examples
#' data("Amount")
#' data1 = Amount[sample(1:nrow(Amount), 1000),]
#' data2 = Amount[sample(1:nrow(Amount), 1000),]
#' abcxyzData1 = computeABCXYZAnalysis(data1, value = "value", item = "item", timestamp = "date")
#' abcxyzData2 = computeABCXYZAnalysis(data2, value = "value", item = "item", timestamp = "date")
#' comparison = compare(abcxyzData1, abcxyzData2)
#' summary(comparison)
#' @export
setMethod("summary", "ABCXYZComparison", function(object, withMissing=FALSE) {

  if (withMissing) {
    if (object@type == "abc") {
      classes = c("A", "B", "C")
    } else if (object@type == "abcxyz") {
      classes = c("AX", "AY", "AZ", "BX", "BY", "BZ", "CX", "CY", "CZ")
    } else {
      classes = c()
    }

    object@data$class.1 = factor(object@data$class.1, levels=classes, labels=classes)
    object@data$class.2 = factor(object@data$class.2, levels=classes, labels=classes)

  }

  summary = xtabs(~ class.1 + class.2, object@data, drop.unused.levels=!withMissing)

  return(summary)
})



#' Plots the result of an ABC/XYZ analysis
#'
#' Plots a graph that shows what percentage of items is responsible for what amount of value.
#'
#' @param x           Object of class \code{ABCXYZData}.
#' @param plot_engine Name of the plot engine ("graphics", "plotly")
#' @param title       Plot title (e.g. 'ABC-Analysis').
#' @param xlab        Label of x-axis (e.g. 'Percentage of Items').
#' @param ylab        Label of y-axis (e.g. 'Percentage of cumulative Value').
#' @param top5lab     Title of the rank of the top 5 items (e.g. 'Items with the highest Value').
#' @param color       List of plot colors (i.e., itemColor, top5Color, aColor, bColor, cColor).
#'                    Default is \code{list(itemColor = "blue", top5Color = "black", aColor = "green", bColor = "orange", cColor = "red")}.
#' @param item        Name of a single column with an identifier, that is displayed in the top-5-ranking.
#'                    Used if the \code{ABCXYZData} object has multiple item columns. If \code{NA} the first item
#'                    column is displayed.
#' @param ...         Further optional parameters for function \code{graphics::plot} or function \code{plotly::plot_ly}.
#' @author Leon Binder \email{leon.binder@@th-deg.de}
#' @author Bernhard Bauer \email{bernhard.bauer@@th-deg.de}
#' @author Michael Scholz \email{michael.scholz@@th-deg.de}
#' @seealso \code{\link{computeABCXYZAnalysis}} \code{\link[=ABCXYZData-class]{ABCXYZData}}
#' @keywords methods
#' @docType methods
#' @examples
#' data("Amount")
#' abcResult = computeABCXYZAnalysis(data = Amount,
#'     value = "value",
#'     item = "item",
#'     timestamp = "date")
#' plot(abcResult,
#'      plot_engine = "graphics",
#'      title = "ABC Analysis",
#'      xlab = "Items",
#'      ylab = "Demand")
#' @export
setMethod("plot", "ABCXYZData", function(x, plot_engine = c("graphics", "plotly"),
                                         title = "", xlab = "", ylab = "", top5lab = NA,
                                         color = list(itemColor = "blue", top5Color = "black", aColor = "green", bColor = "orange", cColor = "red"), item = NA, ...) {
  if (plot_engine == "graphics") {
    .plot_graphics(x, title, xlab, ylab, top5lab, color = color, item, ...)
  } else if (plot_engine == "plotly") {
    .plot_plotly(x, title, xlab, ylab, top5lab, color = color, item, ...)
  }
})


# Plots the result of an ABC/XYZ analysis
#
# Plots a graph that shows what percentage of items is responsible for what amount of value.
#
# @param x          Object of class ABCXYZData.
# @param title      Plot title (e.g. 'ABC-Analysis').
# @param xlab       Label of x-axis (e.g. 'Percentage of Items').
# @param ylab       Label of y-axis (e.g. 'Percentage of cumulative Value').
# @param top5lab    Title of the rank of the top 5 items (e.g. 'Items with the highest Value').
# @param color       List of colors for categories (i.e., aColor, bColor, cColor). Default is list(aColor = "green", bColor = "orange", cColor = "red").
# @param item       Name of a single column with an identifier, that is displayed in the top-5-ranking.
#                   Used if the ABCXYZanalysis object has multiple item columns. If NA the first item
#                   column is displayed.
# @param ...        Further optional parameters for function graphics::plot.
.plot_graphics = function(x, title = "", xlab = "", ylab = "", top5lab = NA, color = list(itemColor = "blue", top5Color = "black", aColor = "green", bColor = "orange", cColor = "red"), item = NA, ...) {
  abcData = x@data[order(x@data$cumulative.percentage),]

  abcData$percentage.items = 1 / nrow(abcData) * 100
  abcData$percentage.items = cumsum(abcData$percentage.items)

  # Basic Plot.
  plot(
    x = abcData$percentage.items,
    y = abcData$cumulative.percentage,
    col = color$itemColor,
    main = title,
    xlab = xlab,
    ylab = ylab,
    xlim = c(0, 100),
    ylim = c(0, 100),
    ... = ...
  )
  # axis(1, at = seq(0, 100, 10))
  # axis(2, at = seq(0, 100, 10))

  # Data for ABC-Class-lines.
  xa = max(abcData[abcData$abc == "A",]$percentage.items, na.rm = TRUE)
  xb = max(abcData[abcData$abc == "B",]$percentage.items)

  ya = max(abcData[abcData$abc == "A",]$cumulative.percentage)
  yb = max(abcData[abcData$abc == "B",]$cumulative.percentage)

  # Lines for ABC-Classes.
  lines(x = c(xa, xa),
        y = c(0, ya),
        col = color$aColor)
  lines(x = c(0, xa),
        y = c(ya,  ya),
        col = color$aColor)
  lines(x = c(0, 0),
        y = c(0, ya),
        col = color$aColor)
  lines(x = c(0, xa),
        y = c(0, 0),
        col = color$aColor)

  lines(x = c(xb, xb),
        y = c(ya, yb),
        col = color$bColor)
  lines(x = c(xa, xb),
        y = c(yb, yb),
        col = color$bColor)
  lines(x = c(xa, xa),
        y = c(ya, yb),
        col = color$bColor)
  lines(x = c(xa, xb),
        y = c(ya, ya),
        col = color$bColor)

  lines(x = c(100, 100),
        y = c(yb, 100),
        col = color$cColor)
  lines(x = c(xb, 100),
        y = c(100, 100),
        col = color$cColor)
  lines(x = c(xb, xb),
        y = c(yb, 100),
        col = color$cColor)
  lines(x = c(xb, 100),
        y = c(yb, yb),
        col = color$cColor)

  # Labels for ABC-Classes.
  text(0 + 2,
       ya + 2,
       labels = "A",
       cex = 1.5,
       col = color$aColor)
  text(xa + 2,
       yb + 2,
       labels = "B",
       cex = 1.5,
       col = color$bColor)
  text(xb + 3,
       100 - 3,
       labels = "C",
       cex = 1.5,
       col = color$cColor)

  # Title and text of the top-5-items.
  if (!is.na(top5lab)) {
    text(
      60,
      60,
      labels = top5lab,
      cex = 1.2,
      col = color$top5Color,
      pos = 4
    )

    # If no item column is specified the first identifying column of the ABCXYZanalysis object
    # is used, otherwise the column item is displayed.
    if (!is.na(item)) {
      labels = paste(1:5, ". ", as.character(head(abcData[[item]], n = 5)), sep = "")
    } else {
      labels = paste(1:5, ". ", as.character(head(abcData[[x@item[1]]], n = 5)), sep = "")
    }

    text(
      c(60, 60, 60, 60, 60),
      c(50, 40, 30, 20, 10),
      labels = labels,
      cex = 1,
      col = color$top5Color,
      pos = 4
    )
  }
}



# Plots the result of an ABC/XYZ analysis with plotly
#
# Plots a graph that shows what percentage of items is responsible for what amount of value.
#
# @param x          Object of class ABCXYZData.
# @param title      Plot title (e.g. 'ABC-Analysis').
# @param xlab       Label of x-axis (e.g. 'Percentage of Items').
# @param ylab       Label of y-axis (e.g. 'Percentage of cumulative Value').
# @param top5lab    Title of the rank of the top 5 items (e.g. 'Items with the highest Value').
# @param color      List of colors for categories (i.e., aColor, bColor, cColor). Default is list(aColor = "green", bColor = "orange", cColor = "red").
# @param item       Name of a single column with an identifier, that is displayed in the top-5-ranking.
#                   Used if the ABCXYZanalysis object has multiple item columns. If NA the first item
#                   column is displayed.
# @param ...        Further optional parameters for function plotly::plot_ly.
.plot_plotly = function(x, title = "", xlab = "", ylab = "", top5lab = NA,
                        color = list(itemColor = "blue", top5Color = "black", aColor = "green", bColor = "orange", cColor = "red"),
                        item = NA, ...) {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Package \"plotly\" needed for this function to work. Please install it.")
  } else {
    abcData = x@data[order(x@data$cumulative.percentage),]
    abcData$percentage.items = 1 / nrow(abcData) * 100
    abcData$percentage.items = cumsum(abcData$percentage.items)

    # Basic Plot.
    if (!is.na(item)) {
      text = abcData[[item]]
    } else {
      text = abcData[[x@item[1]]]
    }

    p = plotly::plot_ly(data = abcData, type = "scatter", mode = "markers", ...=...) %>%
      plotly::add_trace(name = "",
                        x = ~ percentage.items,
                        y = ~ cumulative.percentage,
                        marker = list(color = color$itemColor),
                        text = text,
                        hovertemplate = paste('%{text}<br>', 'X: %{x:.2f} %<br>','Y: %{y:.2f} %')) %>%
      plotly::layout(
        title = title,
        xaxis = list(range = c(0, 105), title = xlab),
        yaxis = list(range = c(0, 105), title = ylab),
        showlegend = FALSE)

    # Data for ABC-Class-lines.
    xa = max(abcData[abcData$abc == "A",]$percentage.items, na.rm = TRUE)
    xb = max(abcData[abcData$abc == "B",]$percentage.items)
    ya = max(abcData[abcData$abc == "A",]$cumulative.percentage)
    yb = max(abcData[abcData$abc == "B",]$cumulative.percentage)

    # Lines for ABC-Classes.
    p = p %>%
      plotly::add_segments(x = 0, xend = 0, y = 0, yend = ya, line = list(color = color$aColor),
                           name = "", hovertemplate = paste('X: %{x:.2f} %, Y: %{y:.2f} %')) %>%
      plotly::add_segments(x = 0, xend = xa, y = 0, yend = 0, line = list(color = color$aColor),
                           name = "", hovertemplate = paste('X: %{x:.2f} %, Y: %{y:.2f} %')) %>%
      plotly::add_segments(x = 0, xend = xa, y = ya, yend = ya, line = list(color = color$aColor),
                           name = "", hovertemplate = paste('X: %{x:.2f} %, Y: %{y:.2f} %')) %>%
      plotly::add_segments(x = xa, xend = xa, y = 0, yend = ya, line = list(color = color$aColor),
                           name = "", hovertemplate = paste('X: %{x:.2f} %, Y: %{y:.2f} %')) %>%

      plotly::add_segments(x = xb, xend = xb, y = ya, yend = yb, line = list(color = color$bColor),
                           name = "", hovertemplate = paste('X: %{x:.2f} %, Y: %{y:.2f} %')) %>%
      plotly::add_segments(x = xa, xend = xb, y = yb, yend = yb, line = list(color = color$bColor),
                           name = "", hovertemplate = paste('X: %{x:.2f} %, Y: %{y:.2f} %')) %>%
      plotly::add_segments(x = xa, xend = xa, y = ya, yend = yb, line = list(color = color$bColor),
                           name = "", hovertemplate = paste('X: %{x:.2f} %, Y: %{y:.2f} %')) %>%
      plotly::add_segments(x = xa, xend = xb, y = ya, yend = ya, line = list(color = color$bColor),
                           name = "", hovertemplate = paste('X: %{x:.2f} %, Y: %{y:.2f} %')) %>%

      plotly::add_segments(x = 100, xend = 100, y = yb, yend = 100, line = list(color = color$cColor),
                           name = "", hovertemplate = paste('X: %{x:.2f} %, Y: %{y:.2f} %')) %>%
      plotly::add_segments(x = xb, xend = 100, y = 100, yend = 100, line = list(color = color$cColor),
                           name = "", hovertemplate = paste('X: %{x:.2f} %, Y: %{y:.2f} %')) %>%
      plotly::add_segments(x = xb, xend = xb, y = yb, yend = 100, line = list(color = color$cColor),
                           name = "", hovertemplate = paste('X: %{x:.2f} %, Y: %{y:.2f} %')) %>%
      plotly::add_segments(x = xb, xend = 100, y = yb, yend = yb, line = list(color = color$cColor),
                           name = "", hovertemplate = paste('X: %{x:.2f} %, Y: %{y:.2f} %'))

    # Labels for ABC-Classes.
    p = p %>%
      plotly::add_annotations(x = 1, y = ya + 3, text = "A", showarrow = F, name = "",
                              font = list(color = color$aColor, size = 14)) %>%
      plotly::add_annotations(x = xa + 1, y = yb + 3, text = "B", showarrow = F, name = "",
                              font = list(color = color$bColor, size = 14)) %>%
      plotly::add_annotations(x = xb + 1, y = 100 + 3, text = "C", showarrow = F, name = "",
                              font = list(color = color$cColor, size = 14))

    # Title and text of the top-5-items.
    if (!is.na(top5lab)) {

      # If no item column is specified the first identifying column of the ABCXYZanalysis object
      # is used, otherwise the column item is displayed.
      if (!is.na(item)) {
        labels = paste(1:5, ". ", as.character(head(abcData[[item]], n = 5)), sep = "")
      } else {
        labels = paste(1:5, ". ", as.character(head(abcData[[x@item[1]]], n = 5)), sep = "")
      }

      p = p %>%
        plotly::add_annotations(x = 60, y = 60, text = top5lab, showarrow = F, name = "",
                                font = list(color = color$top5Color, size = 16)) %>%
        plotly::add_annotations(x = c(60, 60, 60, 60, 60), y = c(50, 40, 30, 20, 10),
                                showarrow = F, text = labels, name = "",
                                font = list(color = color$top5Color, size = 14))
    }

    return(p)
  }
}

