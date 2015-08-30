
#' Transforming a mlydata Object into a Series
#' 
#' Transforming a mlydata object into a series of monthly observations.
#' 
#' 
#' @usage as.series(x)
#' @param x A mlydata object. The month of it must be 1 : 12.
#' @return A series, whose index is a yearmon object definited in the zoo
#' package.
#' @export
#' @name as.series
#' @rdname as.series
as.series <- function(x, ...) { UseMethod('as.series') }

#' @export as.series.mlydata
#' @rdname as.series
#' @examples
#' 
#' x <- matrix(1 : 36, nrow = 3)
#' md <- mlydata(x, year = 1991 : 1993, month = 1 : 12)
#' as.series(md)
#' 
as.series.mlydata <- function(x) {
    if(!all(attr(x, 'month') == 1 : 12)) {
        stop('x must have 12 columns which respresents 12 months.')
    }
    year <- index(x)
    month <- 1 : 12
    yy <- rep(year, each = 12)
    mm <- rep(month, length(year))
    yymm <- yearmon(yy + (mm - 1) / 12)
    mat <- coredata(x)
    vec <- as.vector(t(mat))
    zooObj <- zoo(vec, order.by = yymm)
    return(zooObj)
}



