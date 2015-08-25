
#' Correlation Computing for \code{zoo} or \code{zoocat} Objects
#' 
#' Compute correlations for \code{zoo} or \code{zoocat} objects.
#' 
#' 
#' @usage corr(x, y, ...)
#' @param x A \code{zoo} or \code{zoocat} object.
#' @param y A \code{zoo} object with the same index as \code{x}.
#' @param ...  Other arguments for function \code{cor}.
#' @return For \code{corr.zoo}, the same as in function \code{cor}. For
#' \code{corr.zoocat}, a data frame will be returned.
#' @examples
#' 
#' x <- zoo(c(1, 3, 2, 4, 5))
#' y <- zoo(c(12, 30, 2, 46, 5))
#' corr(x, y)
#' 
#' x <- matrix(1 : 20, nrow = 5)
#' colAttr <- data.frame(month = c(2, 3, 5, 6), name = 'sst')
#' zc <- zoocat(x, order.by = 1991 : 1995, colattr = colAttr)
#' y <- zoo(c(3, 5, 4, 6, 3), order.by = 1991 : 1995)
#' corr(zc, y)
#' 
#' 
#' @export
#' @rdname corr
#' @name corr
corr <- function (x, ...) {
    UseMethod('corr')
}

#' @rdname corr
#' @export
corr.zoo <- function (x, y, ...) {
    stopifnot(is.zoo(x) & is.zoo(y))
    stopifnot(length(index(x)) == length(index(y)))
    stopifnot(all(index(x) == index(y)))
    ret <- cor(coredata(x), coredata(y), ...)
    return(ret)
}


#' @rdname corr
#' @export
corr.zoocat <- function (x, y, ...) {
    stopifnot(inherits(x, 'zoocat'))
    stopifnot(is.zoo(y))
    stopifnot(length(index(x)) == length(index(y)))
    stopifnot(all(index(x) == index(y)))
    if (!is.null(dim(y))) {
        stopifnot(ncol(y) == 1)
    }
    ccvec <- cor(coredata(x), coredata(y), ...)
    cc.df <- data.frame(cattr(x), cor.value = ccvec)
    return(cc.df)
}

