% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ABCXYZanalysis.R
\docType{methods}
\name{compare,ABCXYZData,ABCXYZData-method}
\alias{compare,ABCXYZData,ABCXYZData-method}
\alias{compare,ABCXYZData-method}
\title{Compares the results of two ABC/XYZ analyses}
\usage{
\S4method{compare}{ABCXYZData,ABCXYZData}(
  object1,
  object2,
  valueDiff = NA,
  xyzCoefficientDiff = NA,
  unequalABC = NA,
  unequalXYZ = NA
)
}
\arguments{
\item{object1}{Object of class \code{ABCXYZData}.}

\item{object2}{Object of class \code{ABCXYZData}.}

\item{valueDiff}{Only items with a difference of the column value larger than valueDiff between the
first and second ABC-XYZ-Analysis are returned. In the comparison data.frame a new
column is added for the difference in the value columns.}

\item{xyzCoefficientDiff}{Only items with a difference of the column xyzCoefficient larger than the
xyzCoefficientDiff between the first and second ABC-XYZ-Analysis are returned.
In the comparison data.frame a new column is added for the difference in the
xyzCoefficient columns.}

\item{unequalABC}{If \code{TRUE} only items are returned, where the ABC-Classes are different. If \code{FALSE} only
items are returned, where the ABC-Classes are equal. If \code{NA}, no further restriction
takes place based on the column ABC.}

\item{unequalXYZ}{If \code{TRUE} only items are returned, where the XYZ-Classes are different. If \code{FALSE} only
items are returned, where the XYZ-Classes are equal. If \code{NA}, no further restriction
takes place based on the column XYZ.}
}
\value{
An \code{ABCYXZComparison} object.
}
\description{
Compares the class assignments of two ABC- or two ABC/XYZ analyses.
}
\examples{
data("Amount")
data1 = Amount[sample(1:nrow(Amount), 1000),]
data2 = Amount[sample(1:nrow(Amount), 1000),]
abcxyzData1 = computeABCXYZAnalysis(data1, value = "value", item = "item", timestamp = "date", 
                                    temporalAggregation = "day", XY = 0.5, YZ = 1)
abcxyzData2 = computeABCXYZAnalysis(data2, value = "value", item = "item", timestamp = "date", 
                                    temporalAggregation = "day", XY = 0.5, YZ = 1)
comparison = compare(abcxyzData1, abcxyzData2)
}
\seealso{
\code{\link[=ABCXYZComparison-class]{ABCXYZComparison}}
}
\author{
Leon Binder \email{leon.binder@th-deg.de}

Bernhard Bauer \email{bernhard.bauer@th-deg.de}

Michael Scholz \email{michael.scholz@th-deg.de}
}
\keyword{methods}
