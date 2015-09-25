
#' Coercion Objects to Class \code{zoo}
#' 
#' Coercing objects to class \code{zoo}.
#' 
#' For \code{mlydata} and \code{zoocat}, the returned \code{zoo} object will
#' be added column names automatically. \cr
#' Note that the result of \code{as.zoo} will be a \code{zooreg} object if 
#' the input \code{x} is inherited from \code{zooreg}.
#' 
#' @return A \code{zoo} or \code{zoocat} object.
#' @examples
#' 
#' x <- matrix(1 : 20, nrow = 5)
#' md <- mlydata(x, year = 1991 : 1995, month = c(2, 3, 5, 6))
#' z <- as.zoo(md)
#' 
#' x <- matrix(1 : 20, nrow = 5)
#' md1 <- mlydata(x, year = 1991 : 1995, month = c(2, 3, 5, 6))
#' md2 <- md1 + 1
#' mdList <- mlydataList(list(md1 = md1, md2 = md2))
#' zc <- as.zoocat(mdList)
#' as.zoo(zc)
#' 
#' @rdname as.zoo
#' @name as.zoo
#' @export
#' @param x an object.
#' @param add.colname logical. If TRUE, column names will be added automatically.
#' @param ... Further arguments.
as.zoo.mlydata <- function(x, add.colname = TRUE, ...) {
    class(x) <- class(x)[-1]
    if (add.colname == TRUE) {
        colnames(x) <- month2str(attr(x, 'month'))
    }
    attr(x, 'month') <- NULL
    return(x)
}

#' @rdname as.zoo
#' @export
as.zooreg.mlydata <- function(x, add.colname = TRUE, ...) {
    class(x) <- class(x)[-1]
    if (add.colname == TRUE) {
        colnames(x) <- month2str(attr(x, 'month'))
    }
    attr(x, 'month') <- NULL
    x <- as.zooreg(x)
    return(x)
}

#' @rdname as.zoo
#' @export
as.zoo.zoocat <- function (x, add.colname = TRUE, ...) {
    if (length(x) == 0){
        return(zoo())
    } else {
        if (add.colname == TRUE) {
            colnames(x) <- cattr2str(attr(x, 'cattr'))
        }
        attr(x, 'cattr') <- NULL
        class(x) <- class(x)[-1]
        return(x)
    }
}

#' @rdname as.zoo
#' @export
as.zooreg.zoocat <- function (x, add.colname = TRUE, ...) {
    if (length(x) == 0){
        return(zoo())
    } else {
        if (add.colname == TRUE) {
            colnames(x) <- cattr2str(attr(x, 'cattr'))
        }
        attr(x, 'cattr') <- NULL
        class(x) <- class(x)[-1]
        x <- as.zooreg(x)
        return(x)
    }
}

