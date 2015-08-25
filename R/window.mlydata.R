
#' Get Subset of a mlydata Object
#' 
#' Get a subset of a mlydata object.
#' 
#' @aliases window.mlydata
#' @usage window(x, year = NULL, month = NULL)
#' @param x A \code{mlydata} object.
#' @param year A numeric vector representing years.
#' @param month A numeric vector in which numbers are used to represent months.
#' The elements of \code{month} can be smaller than 1 or larger than 12, which
#' means the months of past years or future years. For example, the months of
#' last year is (1 : 12) - 12.
#' @return A mlydata object.
#' @examples
#' 
#' x <- matrix(1 : 20, nrow = 5)
#' md <- mlydata(x, year = 1991 : 1995, month = c(2, 3, 5, 6))
#' window(md, year = 1992 : 1993, month = c(3, 6))
#' 
#' @export window.mlydata
window.mlydata <- function(x, year = NULL, month = NULL, ...) {
    mon0 <- attr(x, 'month')
    class(x) <- 'zoo'
    if (!is.null(year)) {
        x <- window(x, index. = year, ...)
    } else {
        x <- window(x, ...)
    }
    if (is.null(ncol(x))) {
        x <- zoo(matrix(coredata(x), ncol = 1), order.by = index(x))
    }
    attr(x, 'month') <- mon0
    class(x) <- c('mlydata', 'zoo')
    nMonth <- length(month)
    if(!is.null(month)) {
        idCol <- rep(0, nMonth)
        for(i in 1 : nMonth) {
            colNow <- which(month[i] == attr(x, 'month'))
            if(length(colNow) == 0)
                stop('Some month does not exist in x.')
            idCol[i] <- colNow
        }
    } else {
        idCol <- 1 : ncol(x)
    }
    x <- x[, idCol, drop = FALSE]
    if(!is.null(year))  index(x) <- as.integer(year)
    if(!is.null(month))  attr(x, 'month') <- as.integer(month)
    class(x) <- c('mlydata', 'zoo')
    return(x)
}

