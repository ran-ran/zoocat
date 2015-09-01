
#' The Rolling Means of a mlydata Object by Month
#' 
#' The rolling means of a mlydata object by month.
#' 
#' 
#' previous and current data is in the rolling window.
#' @return A mlydata object.\cr
#' @examples
#' 
#' x <- matrix(1 : 60, nrow = 5)
#' md <- mlydata(x, year = 1991 : 1995)
#' rollmean_by_month(md, k = 2)
#' rollmean_by_month(md, k = 3, onlyUsePrev = FALSE)
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
#' @param onlyUsePrev If TRUE, the rolling window is asymmetric, and only
rollmean_by_month.mlydata <- function (x, k, onlyUsePrev = TRUE) {
    stopifnot(all(attr(x, 'month') == 1 : 12))
    stopifnot(k <= 12)
    if (onlyUsePrev == FALSE & (k%%2 != 1)) {
        stop('K must be odd number when onlyUsePrev is FALSE.')
    }
    xp <- x
    xf <- x
    index(xp) <- index(x) + 1
    index(xf) <- index(x) - 1
    xAll <- cbind(as.zoo(xp), as.zoo(x), as.zoo(xf))
    idCol <- 13 : 24
    xroll <- mlydata(matrix(0, nrow = nrow(xAll), ncol = 12),
                     year = index(xAll))
    for (i in 1 : length(idCol)) {
        colNow <- idCol[i]
        if (onlyUsePrev == TRUE) {
            xroll[, i] <- rowMeans(xAll[, (colNow - k + 1) : colNow, drop = FALSE])
        } else {
            xroll[, i] <- rowMeans(xAll[, (colNow - floor(k/2)) : (colNow + floor(k/2)), drop = FALSE])
        }
    }
    xroll <- na.trim(xroll, sides = 'both', is.na = 'all')
    return(xroll) 
}

