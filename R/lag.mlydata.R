
#' Lags of \code{mlydata} Objects
#' 
#' @examples
#' 
#' x <- matrix(1 : 20, nrow = 5)
#' md <- mlydata(x, year = 1991 : 1995, month = c(2, 3, 5, 6))
#' lag(md, k = 1)
#' lag(md, k = -2)
#' lag(md, k = -1)
#' cbind(lag(md, -1), md)
#' 
#' lag(md, k = -1, adjust.month = FALSE)
#' 
#' @name lag.mlydata
#' @rdname lag.mlydata
#' @param x A \code{zoocat} or \code{mlydata} object.
#' @param k The number of the lags.
#' @param adjust.month Logical. If TRUE, the month of \code{x} will be 
#' add \code{12 * k}.
#' @param ... further arguments.
#' @export
lag.mlydata <- function (x, k = 1, adjust.month = TRUE, ...) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    x <- lag(x, k = k)
    if (adjust.month == TRUE) {
        attr(x, 'month') <- month + 12 * k
    } else {
        attr(x, 'month') <- month
    }
    class(x) <- c('mlydata', class(x))
    return(x)
}


