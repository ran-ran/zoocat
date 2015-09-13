
#' The Rolling Means of a mlydata Object by Month
#' 
#' The rolling means of a mlydata object by month.
#' 
#' 
#' @return A mlydata object.\cr
#' @examples
#' 
#' x <- matrix(1 : 60, nrow = 5, byrow = TRUE)
#' md <- mlydata(x, year = 1991 : 1995)
#' rollmean_by_month(md, k = 3, align = 'left')
#' rollmean_by_month(md, k = 3, align = 'right')
#' rollmean_by_month(md, k = 3)
#' rollmean(md, k = 3)
#' 
#' 
#' @export 
#' @name rollmean_by_month
#' @rdname rollmean_by_month
#' @param x A \code{mlydata} object.
#' @param ... Additional arguments to be passed to or from methods.
rollmean_by_month <- function (x,...) {UseMethod('rollmean_by_month')}


#' @export 
#' @rdname rollmean_by_month
#' @param k The width of the rolling window.
#' @param align Character specifying whether the index of the result 
#' should be left- or right-aligned or centered (default) compared to
#' the rolling window of observations.
rollmean_by_month.mlydata <- function (x, k, align = 'center', ...) {
    stopifnot(all(attr(x, 'month') == 1 : 12))
    zobj <- melt(x, ret = 'zoo')
    zobj <- na.trim(zobj)
    zobj <- rollmean(zobj, k = k, align = align)
    dt <- index(zobj)
    dfobj <- data.frame(year = as.numeric(format(dt, "%Y")),
                        month = as.numeric(format(dt, "%m")),
                        value = coredata(zobj))
    xroll <- cast2mlydata(dfobj, year.var = 'year', value.var = 'value',
                        month.var = 'month')
    xroll <- na.trim(xroll, sides = 'both', is.na = 'all')
    return(xroll) 
}

