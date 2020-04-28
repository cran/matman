# Normalizes a given vector (i.e., scales it in [0, 1]).
#
# x Vector that will be normalized.
.normalize = function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Computes the dissimilarity of an item to all other items
#
# distances Vector of distances to all items.
# i Item for which the dissimilarity is computed.
.dissimilarity = function(distances, i) {
  if (length(distances) <= 1) {
    dis = 0
  } else {
    idx = which(rownames(distances) == i)
    dis = 1 / (nrow(distances) - 1) * sum(distances[idx, ])
  }
  return(dis)
}

# Implements the Macnaughton-Smith et al. clustering algorithm
#
# distances Matrix of distances
.cluster = function(distances) {
  Nl = rep(1, ncol(distances))
  Nu = rep(0, ncol(distances))
  end = F
  while (!end) {
    max = 0
    candidate = NA
    for (i in which(Nl == 1)) {
      dist = .dissimilarity(distances[which(Nl == 1), which(Nl == 1)], i) -
        .dissimilarity(distances[c(i, which(Nu == 1)),
                                 c(i, which(Nu == 1))], i)
      if (dist > max) {
        max = dist
        candidate = i
      }
    }
    if (is.na(candidate)) {
      end = T
    } else {
      Nl[candidate] = 0
      Nu[candidate] = 1
    }
    if (sum(Nl) == 1) {
      end = T
    }
  }
  result = list(Nl, Nu)
  return(result)
}

#' Detects items whose value (stock, demand, etc.) has changed over time
#'
#' Detects items whose value (stock, demand, etc.) has changed over time in contrast to other items.
#' This analysis is based on the Macnaughton-Smith et al. clustering algorithm.
#'
#' @param data Data frame that will be expanded.
#' @param value Name of the column variable that contains the value for the ABC and XYZ analysis.
#' @param item Name of the column including the item names or identifiers (e.g., product name, EAN)
#' @param timestamp Name of the column including the timestamp. This column should be in POSIX or Date-format.
#' @param temporalAggregation Temporal aggregation mode (i.e., "day", "week", "month", "quarter", "year").
#' @param aggregationFun Function for aggregating the value column. Default is sum.
#' @param preProcess A string vector that defines a pre-processing of the aggregated data before clustering.
#' Available pre-processing methods are "center", "scale", "standardize", and "normalize". Default is NA (no pre-processing).
#' @param recentTimePeriods Integer indicating the number of time periods that are used to define the recent
#' item values. Default is 5.
#' @return Returns a data frame showing to which cluster each item belongs based on all value and based on the recent values
#' as well as whether the item has switched the cluster.
#' @references Macnaughton-Smith, P., Williams, W.T., Dale, M.B., Mockett, L.G. (1964)
#' "Dissimilarity Analysis: a new Technique of Hierarchical Sub-division",
#' \emph{Nature}, \strong{202}, 1034--1035.
#' @examples
#' data("Amount")
#' timeVariations = detectTimeVariations(data = Amount,
#'     value = "amount",
#'     item = "item",
#'     timestamp = "date",
#'     temporalAggregation = "week")
#' @export
detectTimeVariations = function(data,
                                value,
                                item,
                                timestamp,
                                temporalAggregation = c("day", "week", "month", "quarter", "year"),
                                aggregationFun = sum,
                                preProcess = NA,
                                recentTimePeriods = 5) {
  if (!value %in% names(data)) {
    stop(paste("Data does not include column ", value, ".", sep = ""))
  }
  if (!all(item %in% names(data))) {
    stop(paste("Data does not include columns ", item, ".", sep = ""))
  }
  if (!timestamp %in% names(data)) {
    stop(paste("Data does not include column ", timestamp, ".", sep = ""))
  }
  if (!is.numeric(recentTimePeriods) || recentTimePeriods <= 2) {
    warning("recentTimePeriods is set to 5")
    recentTimePeriods = 5
  }
  itemData = aggregateData(data = data,
                       value = value,
                       item = item,
                       timestamp = timestamp,
                       temporalAggregation = temporalAggregation,
                       aggregationFun = aggregationFun)
  itemData = itemData %>% spread(key = item, value = value)
  itemData = itemData[,-1]
  itemData[is.na(itemData)] = 0
  if (!is.na(preProcess)) {
    if (preProcess == "center") {
      itemData = itemData %>% mutate_all(.funs = scale, scale = F) %>% as.data.table
    } else if (preProcess == "scale") {
      itemData = itemData %>% mutate_all(.funs = scale, center = F) %>% as.data.table
    } else if (preProcess == "standardize") {
      itemData = itemData %>% mutate_all(.funs = scale) %>% as.data.table
    } else if (preProcess == "normalize") {
      itemData = itemData %>% mutate_all(.funs = .normalize) %>% as.data.table
    }
  }
  names(itemData) = as.character(seq(1, ncol(itemData), 1))
  allDist = as.matrix(dist(t(itemData), upper = T, diag = T))
  allCluster = .cluster(allDist)

  recentItemData = tail(itemData, recentTimePeriods)
  recentDist = as.matrix(dist(t(recentItemData), upper = T, diag = T))
  recentCluster = .cluster(recentDist)

  items = unique(data[[item]])
  all = allCluster[[1]] + allCluster[[2]] * 2
  recent = recentCluster[[1]] + recentCluster[[2]] * 2
  changes = all != recent
  result = data.frame(item = items, clusterAllData = all, clusterRecentData = recent, changes = changes)
  return(result)
}
