
#' Transforming a \code{zoomly} Object to be Seasonal Data
#' 
#' Transforming a \code{zoomly} object to be seasonal data.
#' 
#' 
#' @return A \code{zoo} object.
#' @examples
#' 
#' x <- matrix(1 : 36, nrow = 3, byrow = TRUE)
#' zm <- zoomly(x, year = 1991 : 1993, month = 1 : 12)
#' as.seasonal(zm)
#' as.seasonal(zm, DJF.first = FALSE)
#' 
#' @export
#' @name as.seasonal
#' @rdname as.seasonal
#' @param x A \code{zoomly} object.
#' @param ... Additional arguments to be passed to or from methods.
as.seasonal <- function (x, ...) {
    UseMethod('as.seasonal')
}


#' @rdname as.seasonal
#' @export 
#' @param DJF.first If TRUE, the winter season will be in the first column.
as.seasonal.zoomly <- function (x, DJF.first = TRUE, ...) {
    stopifnot(all(mon(x) == 1 : 12))
    x <- as.zoo(x)
    if (DJF.first == TRUE) {
        xDec <- x[, 12, drop = FALSE]
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


