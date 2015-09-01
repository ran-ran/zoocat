
#' Correlation Computing for \code{zoo} or \code{zoocat} Objects
#' 
#' Compute correlations for \code{zoo} or \code{zoocat} objects.
#' 
#' @examples
#' 
#' x <- zoo(c(1, 3, 2, 4, 5))
#' y <- zoo(c(12, 30, 2, 46, 5))
#' corr(x, y)
#' 
#' x <- matrix(1 : 20, nrow = 5)
#' md <- mlydata(x, year = 1991 : 1995, month = c(2, 3, 5, 6))
#' corr(md, md)
#' corr(md)
#' corr(md, method = 'kendall')
#' 
#' x <- matrix(1 : 20, nrow = 5)
#' colAttr <- data.frame(month = c(2, 3, 5, 6), name = 'x')
#' zc <- zoocat(x, order.by = 1991 : 1995, colattr = colAttr)
#' y <- zoo(c(3, 5, 4, 6, 3), order.by = 1991 : 1995)
#' corr(zc, y, method = 'kendall')
#' corr(zc, md)
#' corr(zc, method = 'kendall')
#' 
#' 
#' @export
#' @rdname corr
#' @name corr
#' @param x A \code{zoo} or \code{zoocat} object.
#' @param y NULL or a \code{zoo} or \code{zoocat} object. If NULL, x will be used.
#' @param ... Other arguments for function \code{cor}.
corr <- function (x, y = NULL, ...) {
    UseMethod('corr')
}

#' @rdname corr
#' @export
corr.zoo <- function (x, y = NULL, ...) {
    stopifnot(is.null(y) | is.zoo(y))
    if (!is.null(y)) {
        stopifnot(length(index(x)) == length(index(y)))
        stopifnot(all(index(x) == index(y)))
        if (is.null(dim(y))) {
            y <- as.vector(y)
        } else {
            y <- as.matrix(y)
        }
    }
    x <- as.matrix(x)
    ret <- cor(x, y, ...)
    return(ret)
}


#' @rdname corr
#' @export
corr.zoocat <- function (x, y = NULL, ...) {
    stopifnot(is.null(y) | is.zoo(y))
    if (!is.null(y)) {
        stopifnot(length(index(x)) == length(index(y)))
        stopifnot(all(index(x) == index(y)))
        if (is.null(dim(y))) {
            y <- as.vector(y)
        } else {
            y <- as.matrix(y)
        }
    }
    cAttr <- cattr(x)
    x <- as.matrix(x)
    if (!is.null(y) & (is.null(dim(y)) || ncol(y) == 1)) {
        ccvec <- cor(x, y, ...)
        ret <- data.frame(cAttr, cor.value = ccvec)
    } else {
        ret <- cor(x, y, ...)
    }
    return(ret)
}

