#' Aggregate \code{zoocat} object
#' 
#' Aggregate columns of \code{zoocat} object by given column attributes fields. 
#'
#' @export
#' @examples 
#' data(sst)
#' sstmelt <- melt(sst, id.vars = c('year', 'month'))
#' zc <- cast2zoocat(sstmelt, index.var = 'year', value.var = 'value') 
#' zc.agg1 <- aggregate(zc, by = 'month')
#' zc.agg2 <- aggregate(zc, by = 'variable')
#' 
#' @param x a \code{zoocat} object.
#' @param by a character string indicates the field of column attributes.
#' @param FUN a function to be applied to all data subsets.
#' @param ... additional arguments to be passed to methods.
aggregate.zoocat <- function (x, by, FUN = mean, ...) {
    df.melt <- melt(x, value.name = 'value', index.name = 'index')
    str <- paste(by, collapse = '+')
    str <- paste(str, '+', 'index', sep = '')
    str <- paste('value ~', str, sep = '')
    fml <- as.formula(str)
    df.aggr <- aggregate(fml, df.melt, FUN = FUN)
    zcast <- cast2zoocat(df.aggr, index.var = 'index', value.var = 'value')
    return(zcast)
}
