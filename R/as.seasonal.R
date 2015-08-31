
#' Transforming a \code{mlydata} Object to be Seasonal Data
#' 
#' Transforming a \code{mlydata} object to be seasonal data.
#' 
#' 
#' @return A \code{zoo} object.
#' @examples
#' 
#' x <- matrix(1 : 36, nrow = 3, byrow = TRUE)
#' md <- mlydata(x, year = 1991 : 1993, month = 1 : 12)
#' as.seasonal(md)
#' as.seasonal(md, DJF.first = FALSE)
#' 
#' @export
#' @name as.seasonal
#' @rdname as.seasonal
#' @param x A \code{mlydata} object.
#' @param ... Additional arguments to be passed to or from methods.
as.seasonal <- function (x, ...) {
    UseMethod('as.seasonal')
}


#' @rdname as.seasonal
#' @export 
#' @param DJF.first If TRUE, the winter season will be in the first column.
as.seasonal.mlydata <- function (x, DJF.first = TRUE) {
    stopifnot(all(attr(x, 'month') == 1 : 12))
    if (DJF.first == TRUE) {
        xDec <- x[, 12]
        index(xDec) <- index(xDec) + 1
        xBind <- cbind(xDec, x[, 1 : 11])
        for (i in 1 : 4) {
            if (i == 1) {
                ret <- row_means(xBind[, 1 : 3])
            } else {
                tpt <- row_means(xBind[, ((i-1)*3+1) : (i*3)])
                ret <- cbind(ret, tpt)
            }
        }
        colnames(ret) <- c('DJF', 'MAM', 'JJA', 'SON')
    } else {
        xJF <- x[, 1 : 2]
        index(xJF) <- index(xJF) - 1
        xBind <- cbind(x[, 3 : 12], xJF)
        for (i in 1 : 4) {
            if (i == 1) {
                ret <- row_means(xBind[, 1 : 3])
            } else {
                tpt <- row_means(xBind[, ((i-1)*3+1) : (i*3)])
                ret <- cbind(ret, tpt)
            }
        }
        colnames(ret) <- c('MAM', 'JJA', 'SON', 'DJF')
    }
    ret <- na.trim(ret, sides = 'both', is.na = 'all')
    return(ret)

}


