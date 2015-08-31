
#' Cast a Data Frame as a \code{mlydata} Object
#' 
#' Cast a data frame as a \code{mlydata} object.
#' 
#' @param x A data frame.
#' @param index.var The name of the column to be treated as the index of the mlydata object.
#' @param value.var The name of the column which stores the values.
#' @param month.var The name of the column which stores month.
#' @param fun.aggr Aggregation function needed if variables do not identify a single observation
#' for each output cell. Defaults to length (with a message) if needed but not specified.
#' @return A \code{mlydata} object.
#' @examples
#' 
#' x <- data.frame(year = rep(1991 : 1993, each = 3), month = rep(3 : 1, 3), value = 1 : 9)
#' md <- cast2mlydata(x, index.var = 'year', month.var = 'month', value.var = 'value', fun.aggr = mean)
#'
#' @export
cast2mlydata <- function (x, index.var, value.var, month.var, fun.aggr = NULL) {
    zcat <- cast2zoocat(x, index.var = index.var, value.var, attr.var = month.var,
                        fun.aggr = fun.aggr)
    md <- mlydata(coredata(zcat), year = index(zcat), month = cattr(zcat)[, 1])
    return(md)
}