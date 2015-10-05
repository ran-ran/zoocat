
#' Get Subset of a \code{zoomly} Object
#' 
#' Get a subset of a \code{zoomly} object.
#' 
#' @name window
#' @rdname window
#' @param x A \code{zoomly} object.
#' @param year A numeric vector representing years.
#' @param month A numeric vector in which numbers are used to represent months.
#' The elements of \code{month} can be smaller than 1 or larger than 12, which
#' means the months of past years or future years. For example, the months of
#' last year is (1 : 12) - 12.
#' @param ... Other arguments for \code{window.zoo}.
#' @return A \code{zoomly} object.
#' @examples
#' 
#' x <- matrix(1 : 20, nrow = 5)
#' zm <- zoomly(x, year = 1991 : 1995, month = c(2, 3, 5, 6))
#' window(zm, year = 1992 : 1993, month = c(3, 6))
#' window(zm, start = 1993)
#' 
#' 
#' data(sst)
#' zmList <- cast2zoomly(sst, value.var = c('nino12', 'nino3'))
#' window(zmList, year = 1992:1993, month = 3:5)
#' 
#' @export
window.zoomly <- function(x, year = NULL, month = NULL, ...) {
    class0 <- class(x)
    mon0 <- attr(x, 'cattr')
    class(x) <- c('zooreg', 'zoo')
    if (!is.null(year)) {
        x <- window(x, index. = year, ...)
    } else {
        x <- window(x, ...)
    }
    attr(x, 'cattr') <- mon0
    mon0 <- mon0[, 1]
    class(x) <- class0
    nMonth <- length(month)
    if(!is.null(month)) {
        idCol <- rep(0, nMonth)
        for(i in 1 : nMonth) {
            colNow <- which(month[i] == mon0)
            if(length(colNow) == 0)
                stop('Some month does not exist in x.')
            idCol[i] <- colNow
        }
    } else {
        idCol <- 1 : ncol(x)
    }
    x <- x[, idCol, drop = FALSE]
    return(x)
}




