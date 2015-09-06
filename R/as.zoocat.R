
#' Coercing Objects to Class \code{zoocat}
#' 
#' Coercing objects to class \code{zoocat}.
#' 
#' @return A \code{zoocat} object.
#' @examples
#' 
#' x <- matrix(1 : 20, nrow = 5)
#' md <- mlydata(x, year = 1991 : 1995, month = c(2, 3, 5, 6))
#' as.zoocat(md)
#' as.zoocat(md, addname = FALSE)
#' as.zoocat(md, varname = 'x')
#' 
#' mat <- matrix(1 : 20, nrow = 5)
#' x <- mlydata(x, year = 1991 : 1995, month = c(2, 3, 5, 6))
#' y <- x + 100
#' mdList <- mlydataList(list(x = x, y = y))
#' as.zoocat(mdList)
#' 
#' zobj <- zoo(matrix(1:10, nrow = 5), order.by = 11:15)
#' colnames(zobj) <- c('a', 'b')
#' as.zoocat(zobj)
#' 
#' @name as.zoocat
#' @rdname as.zoocat
#' @export
#' @param x An object.
#' @param ... Additional arguments to be passed to or from methods.
as.zoocat <- function (x, ...) { UseMethod('as.zoocat') }

#'
#' @export
#' @rdname as.zoocat
#' @param addname Logical. If TRUE, a field of \code{cattr} "name" will be add.
#' @param varname The value for the name field in the \code{cattr} of 
#' the output \code{zoocat} object. Only valid when \code{addname} is TRUE.
#' If NULL, the variable name will be used.
as.zoocat.mlydata <- function (x, addname = TRUE, varname = NULL, ...) {
    if (addname == TRUE) {
        if (is.null(varname)) {
            sysN <- sys.nframe()
            cl <- sys.call(sysN - 1)
            varname <- as.character(cl[2])
        }
        cAttr <- data.frame(name = varname, month = attr(x, 'month'), stringsAsFactors = FALSE)
    } else {
        cAttr <- data.frame(month = attr(x, 'month'), stringsAsFactors = FALSE)
    }
    year <- index(x)
    z <- zoocat(coredata(x), order.by = year, colattr = cAttr,
                frequency = 1)
    return(z)
}


#'
#' @export
#' @rdname as.zoocat
as.zoocat.mlydataList <- function (x, ...) {
    varname <- names(x)
    for (i in 1 : length(x)) {
        x[[i]] <- as.zoocat(x[[i]], varname[i])
    }
    x <- unclass(x)
    zc <- do.call(merge, args = x)
    return(zc)
}


#' @export
#' @rdname as.zoocat
#' @param colattr The column attribute table for x.
as.zoocat.zoo <- function (x, colattr = NULL, ...) {
    stopifnot(length(dim(x))== 2)
    if (is.null(colattr)) {
        stopifnot(!is.null(colnames(x)))
        colattr <- data.frame(name = colnames(x))
    }
    attr(x, 'cattr') <- colattr
    class(x) <- c('zoocat', class(x))
    return(x)
}


