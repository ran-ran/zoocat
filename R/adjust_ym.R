
#' Adjust Values of Year and Month of a \code{zoocat} or \code{mlydata} Object
#' 
#' Adjust the values of the year and month of a \code{zoocat} or \code{mlydata} object.
#' For \code{zoocat} objects, this method is only meaningful
#' when index of the object is the year and there is a column of \code{cattr}
#' named "month".
#' 
#' For \code{zoocat}, the index year of the object will be added k, and the
#' \code{month} column of \code{cattr} will minus k * 12. The negative month
#' values mean month of previous years with regard to the index year. For
#' \code{mlydata} it is similar.
#' 
#' @examples
#' 
#' x <- matrix(1 : 20, nrow = 5)
#' colAttr <- data.frame(month = c(2, 3, 5, 6), name = 'sst')
#' zc <- zoocat(x, order.by = 1991 : 1995, colattr = colAttr)
#' adjust_ym(zc, k = 1)
#' adjust_ym(zc, k = 2)
#' 
#' x <- matrix(1 : 20, nrow = 5)
#' md <- mlydata(x, year = 1991 : 1995, month = c(2, 3, 5, 6))
#' adjust_ym(md, k = 1)
#' adjust_ym(md, k = 2)
#' 
#' @export
#' @name adjust_ym
#' @rdname adjust_ym
#' @param x A \code{zoocat} or \code{mlydata} object.
#' @param ... Additional arguments to be passed to or from methods.
adjust_ym <- function (x, ...) { UseMethod('adjust_ym') }

#' @export
#' @rdname adjust_ym
#' @param k The number of the lag. See details.
adjust_ym.mlydata <- function (x, k = 1) {
    ynew <- index(x) + k
    index(x) <- ynew
    attr(x, 'month') <- attr(x, 'month') - 12 * k
    return(x)
}

#' @export
#' @rdname adjust_ym
adjust_ym.zoocat <- function (x, k = 1) {
    ynew <- index(x) + k
    if (is.integer(index(x))) {
        ynew <- as.integer(ynew)
    }
    index(x) <- ynew
    attr(x, 'cattr')[, 'month'] <- attr(x, 'cattr')[, 'month'] - 12 * k
    return(x)
}


