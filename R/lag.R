
#' Lags of \code{zoomly} Objects
#' 
#' @name lag.zoomly
#' @rdname lag.zoomly
#' @param x A \code{zoocat} or \code{zoomly} object.
#' @param k The number of the lags.
#' @param adjust.month Logical. If TRUE, the month of \code{x} will be 
#' add \code{12 * k}.
#' @param ... further arguments.
#' @examples
#' 
#' x <- matrix(1 : 20, nrow = 5)
#' zm <- zoomly(x, year = 1991 : 1995, month = c(2, 3, 5, 6))
#' lag(zm, k = 1)
#' lag(zm, k = -2)
#' lag(zm, k = -1)
#' cbind(lag(zm, -1), zm)
#' 
#' lag(zm, k = -1, adjust.month = FALSE)
#' 
#' @export
lag.zoomly <- function (x, k = 1, adjust.month = TRUE, ...) {
    month <- mon(x)
    x <- as.zoo(x, add.colname = FALSE)
    x <- lag(x, k = k)
    if (adjust.month == TRUE) {
        attr(x, 'cattr') <- data.frame(month = month + 12 * k)
    } else {
        attr(x, 'cattr') <- data.frame(month = month)
    }
    class(x) <- c('zoomly', 'zoocat', class(x))
    return(x)
}


#' @export 
'lag.zoocat' <- function (x, ...) {
    colAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- 'lag'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', class(ret))
    return(ret)
}
