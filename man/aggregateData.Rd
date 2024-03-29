% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DataPreparation.R
\name{aggregateData}
\alias{aggregateData}
\title{Performs a temporal aggregation of a data frame}
\usage{
aggregateData(
  data,
  value = NULL,
  item,
  timestamp,
  temporalAggregation = c("day", "week", "month", "quarter", "year", "total"),
  fiscal = 1,
  aggregationFun = sum
)
}
\arguments{
\item{data}{Data frame or matrix on which the ABC analysis is performed.}

\item{value}{Name(s) of the column variable(s) that contains the values for the ABC and XYZ analysis.}

\item{item}{Names of the columns including the item names or identifiers (e.g., product name, EAN).}

\item{timestamp}{Name of the column including the timestamp. This column should be in POSIX or Date-format.}

\item{temporalAggregation}{Temporal aggregation mode for the XYZ-analysis. Possible modes are 'day', 'week', 'month', 'quarter', 'year',
and 'total'. Total only aggregates by item whereas the other modes aggregate by item an temporal unit.}

\item{fiscal}{consider the start of the business year. Default is set to 1 (January)}

\item{aggregationFun}{Function for aggregating the value column. Default is \code{sum}.}
}
\value{
Returns a data frame with the aggregated data with the columns of item, timestamp and sum,
which is the sum of the value column.
}
\description{
Aggregates a data frame based on a timestamp column to days, weeks, months, quarters, years or total.
}
\examples{
data('Amount')
aggregatedData = aggregateData(data = Amount,
    value = "value",
    item = "item",
    timestamp = "date",
    temporalAggregation = "quarter")
}
\seealso{
\code{\link{expandData}}
}
\author{
Leon Binder \email{leon.binder@th-deg.de}

Bernhard Bauer \email{bernhard.bauer@th-deg.de}

Michael Scholz \email{michael.scholz@th-deg.de}
}
