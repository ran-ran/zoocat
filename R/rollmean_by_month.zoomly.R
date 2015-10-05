
#' The Rolling Means of a zoomly Object by Month
#' 
#' The rolling means of a zoomly object by month.
#' 
#' 
#' @return A zoomly object.\cr
#' @examples
#' 
#' x <- matrix(1 : 60, nrow = 5, byrow = TRUE)
#' md <- zoomly(x, year = 1991 : 1995)
#' rollmean_by_month(md, k = 3, align = 'left')
#' rollmean_by_month(md, k = 3, align = 'right')
#' rollmean_by_month(md, k = 3)
#' rollmean(md, k = 3)
#' 
#' 
#' @export 
#' @name rollmean_by_month
#' @rdname rollmean_by_month
#' @param x A \code{zoomly} object.
#' @param ... Additional arguments to be passed to or from methods.
rollmean_by_month <- function (x,...) {UseMethod('rollmean_by_month')}


#' @export 
#' @rdname rollmean_by_month
#' @param k The width of the rolling window.
#' @param align Character specifying whether the index of the result 
#' should be left- or right-aligned or centered (default) compared to
#' the rolling window of observations.
rollmean_by_month.zoomly <- function (x, k, align = 'center', ...) {
    stopifnot(all(attr(x, 'month') == 1 : 12))
    zobj <- melt(x, ret = 'zoo')
    zobj <- na.trim(zobj)
    zobj <- rollmean(zobj, k = k, align = align)
    dt <- index(zobj)
    dfobj <- data.frame(year = as.numeric(format(dt, "%Y")),
                        month = as.numeric(format(dt, "%m")),
                        value = coredata(zobj))
    xroll <- cast2zoomly(dfobj, year.var = 'year', value.var = 'value',
                        month.var = 'month')
    xroll <- na.trim(xroll, sides = 'both', is.na = 'all')
    return(xroll) 
}

