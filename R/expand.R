
#' Expand \code{mlydata} objects
#' 
#' @name expand
#' @rdname expand
#' @export
expand <- function (x, ...) {
    UseMethod('expand')
}


#' @rdname expand
#' 
#' @examples
#' 
#' x <- matrix(1 : 48, nrow = 4)
#' md <- mlydata(x, year = 1991 : 1994)
#' expand(md, k = 1, direction = 'left')
#' expand(md, k = 1, direction = 'right')
#' expand(md, k = 1, direction = 'both')
#' expand(md, k = 2, direction = 'both')
#' 
#' @param x the \code{mlydata} object.
#' @param k the number of cycle to be expanded.
#' @param direction the direction to expand. Can be "left",
#' "right" or "both".
#' @return a \code{mlydata} object.
expand.mlydata <- function (x, k, direction = 'both') {
    stopifnot(direction %in% c('left', 'right', 'both'))
    stopifnot(k >= 1)
    if (direction == 'left') {
        lagvec <- seq(from = -k, to = 0, by = 1)
    } else if (direction == 'right') {
        lagvec <- seq(from = 0, to = k, by = 1)
    } else if (direction == 'both') {
        lagvec <- seq(from = -k, to = k, by = 1)
    }
    ret <- lag(x, k = lagvec[1])
    stopifnot(length(lagvec) >= 2)
    for (i in 2 : length(lagvec)) {
        ret <- merge(ret, lag(x, k =lagvec[i]))
    }
    return(ret)
}




