#' Amount data
#'
#' A dataset containing 23 items and their amounts over 3 years of data.
#'
#' @format A data frame with 10,000 rows and 9 variables:
#' \describe{
#'   \item{date}{Date in format yyyy-mm-dd}
#'   \item{week}{Date in format yyyy-'W'ww}
#'   \item{month}{Date in format yyyy-mm}
#'   \item{quarter}{Date in format yyyy-'Q'q}
#'   \item{year}{Date in format yyyy}
#'   \item{item}{Item ID}
#'   \item{itemgroup}{Item group ID}
#'   \item{amount}{Item amount}
#'   \item{value}{Item value}
#' }
#' @source anonymized real data
"Amount"

#' Stock data
#'
#' A dataset containing 10 items and their stocks over 3 years of data.
#'
#' @format A data frame with 1,610 rows and 5 variables:
#' \describe{
#'   \item{date}{Date in format yyyy-mm-dd}
#'   \item{item}{Item ID}
#'   \item{stock}{Item stock value}
#'   \item{minstock}{Minimum stock per item}
#'   \item{reorderlevel}{Stock threshold for triggering item reorders}
#' }
#' @source anonymized real data
"Stocks"
