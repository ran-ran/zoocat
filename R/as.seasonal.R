
#' Transforming a mlydata Object to be Seasonal Data
#' 
#' Transforming a mlydata object to be seasonal data.
#' 
#' 
#' @usage as.seasonal(x, DJF.first = TRUE)
#' @param x A \code{mlydata} object.
#' @param DJF.first If TRUE, the winter season will be in the first column.
#' @return A \code{zoo} object.
#' @examples
#' 
#' x <- matrix(1 : 36, nrow = 3)
#' md <- mlydata(x, year = 1991 : 1993, month = 1 : 12)
#' print(as.seasonal(md))
#' 
#' @export
#' @name as.seasonal
#' @rdname as.seasonal
as.seasonal <- function (x, ...) {
    UseMethod('as.seasonal')
}


#' @rdname as.seasonal
#' @export as.seasonal.mlydata
as.seasonal.mlydata <- function (x, DJF.first = TRUE) {
    stopifnot(all(attr(x, 'month') == 1 : 12))
    if (DJF.first == TRUE) {
        xDec <- x[, 12]
        xDec <- adjust_ym(xDec)
        xBind <- cbind(xDec, x[, 1 : 11])
        for (i in 1 : 4) {
            if (i == 1) {
                ret <- rmeans(xBind[, 1 : 3])
            } else {
                tpt <- rmeans(xBind[, ((i-1)*3+1) : (i*3)])
                ret <- cbind(ret, tpt)
            }
        }
        colnames(ret) <- c('DJF', 'MAM', 'JJA', 'SON')
    } else {
        xJF <- x[, 1 : 2]
        index(xJF) <- index(xJF) - 1
        if (is.integer(index(x))) {
           index(xJF) <- as.integer(index(xJF))
        }
        xBind <- cbind(x[, 3 : 12], xJF)
        for (i in 1 : 4) {
            if (i == 1) {
                ret <- rmeans(xBind[, 1 : 3])
            } else {
                tpt <- rmeans(xBind[, ((i-1)*3+1) : (i*3)])
                ret <- cbind(ret, tpt)
            }
        }
        colnames(ret) <- c('MAM', 'JJA', 'SON', 'DJF')
    }
    ret <- na.trim(ret, sides = 'both', is.na = 'all')
    return(ret)

}


