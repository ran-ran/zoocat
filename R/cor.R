
#' Correlation Computing for \code{zoo} or \code{zoocat} Objects
#' 
#' Compute corelations for \code{zoo} or \code{zoocat} objects.
#' 
#' @examples
#' 
#' x <- zoo(c(1, 3, 2, 4, 5))
#' y <- zoo(c(12, 30, 2, 46, 5))
#' cor(x, y)
#' 
#' x <- matrix(1 : 20, nrow = 5)
#' colAttr <- data.frame(month = c(2, 3, 5, 6), name = 'x')
#' zc <- zoocat(x, order.by = 1991 : 1995, colattr = colAttr)
#' y <- zoo(c(3, 5, 4, 6, 3), order.by = 1991 : 1995)
#' cor(zc, y, method = 'kendall')
#' cor(zc, method = 'kendall')
#' 
#' 
#' @export
#' @rdname cor
#' @name cor
#' @param x A \code{zoo} or \code{zoocat} object.
#' @param y NULL or a \code{zoo} or \code{zoocat} object. If NULL, x will be used.
#' @param ... Other arguments for function \code{cor}.
cor <- function (x, y = NULL, ...) {
    UseMethod('cor')
}


#' @export
cor.default <- function (x, y = NULL, ...) {
    stats::cor(x, y, ...)
}

#' @rdname cor
#' @export
cor.zoo <- function (x, y = NULL, ...) {
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
    if (is.null(dim(x))) {
        x <- as.vector(x)
    } else {
        x <- as.matrix(x)
    }
    ret <- stats::cor(x, y, ...)
    return(ret)
}


#' @rdname cor
#' @export
cor.zoocat <- function (x, y = NULL, ...) {
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
        ccvec <- stats::cor(x, y, ...)
        ccvec <- as.vector(ccvec)
        ret <- data.frame(cAttr, cor.value = ccvec)
        rownames(ret) <- NULL
    } else {
        ret <- stats::cor(x, y, ...)
    }
    return(ret)
}

