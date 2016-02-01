#' Aggregate \code{zoocat} object by column attributes
#' 
#' Aggregate columns of \code{zoocat} object by given column attributes fields. 
#'
#' @export
#' @examples 
#' data(nino)
#' nino.melt <- melt(nino, id.vars = c('year', 'month'))
#' zc <- cast2zoocat(nino.melt, index.var = 'year', value.var = 'value') 
#' zc.agg1 <- aggregate_col(zc, by = 'month')
#' zc.agg2 <- aggregate_col(zc, by = 'variable')
#' 
#' @param x a \code{zoocat} object.
#' @param by a character string indicates the field of column attributes.
#' @param FUN a function to be applied to all data subsets.
#' @param ... additional arguments to be passed to methods.
aggregate_col <- function (x, by, FUN = mean, ...) {
    stopifnot(inherits(x, 'zoocat'))
    df.melt <- melt(x, value.name = 'value', index.name = 'index')
    str <- paste(by, collapse = '+')
    str <- paste(str, '+', 'index', sep = '')
    str <- paste('value ~', str, sep = '')
    fml <- as.formula(str)
    df.aggr <- aggregate(fml, df.melt, FUN = FUN, ...)
    zcast <- cast2zoocat(df.aggr, index.var = 'index', value.var = 'value')
    return(zcast)
}
