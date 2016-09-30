#' Aggregate \code{zoocat} object by column attributes
#' 
#' Aggregate columns of \code{zoocat} object by given column attributes fields. 
#'
#' @export
#' @examples 
#' # This is the air quality example from package reshape2
#' names(airquality) <- tolower(names(airquality))
#' aqm <- melt(airquality, id = c("month", "day"), na.rm=TRUE) 
#' zc <- cast2zoocat(aqm, index.var = 'month', value.var = 'value', fun.aggregate = mean) 
#' zc.aggr <- aggregate_col(zc, by = 'variable', FUN = max)
#' 
#' @param x a \code{zoocat} object.
#' @param by a character string indicates the field of column attributes.
#' @param FUN a function to be applied to all data subsets.
#' @param ... additional arguments to be passed to methods.
aggregate_col <- function (x, by, FUN = mean, ...) {
    if (!inherits(x, 'zoocat')) {
        stop('x must be a zoocat object.')
    }
    df.melt <- melt(x, value.name = 'value', index.name = 'index')
    str <- paste(by, collapse = '+')
    str <- paste(str, '+', 'index', sep = '')
    str <- paste('value ~', str, sep = '')
    fml <- as.formula(str)
    df.aggr <- aggregate(fml, df.melt, FUN = FUN, ...)
    zcast <- cast2zoocat(df.aggr, index.var = 'index', value.var = 'value')
    return(zcast)
}
