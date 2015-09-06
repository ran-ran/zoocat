

#' Getting or Setting the Column Attributes of a \code{zoocat} Object.
#' 
#' Getting or setting the column attributes of a \code{zoocat} object. Using \code{cattr(x) <- value} 
#' can translate a \code{zoo} object to a \code{zoocat} object.
#' 
#' @examples
#' 
#' x <- matrix(1 : 20, nrow = 5)
#' colAttr <- data.frame(month = c(2, 3, 5, 6), name = 'x')
#' zc <- zoocat(x, order.by = 1991 : 1995, colattr = colAttr)
#' cattr(zc)[, 2] <- 'y'
#' 
#' zobj <- zoo(x, order.by = 1991 : 1995)
#' cattr(zobj) <- colAttr
#' 
#' @name cattr
#' @rdname cattr
#' @export
#' @param x A \code{zoocat} or \code{zoo} object.
#' @param value The new value.
'cattr<-' <- function (x, value) {
    UseMethod('cattr<-')
}


#' @rdname cattr
#' @export
'cattr<-.zoocat' <- function (x, value) {
    stopifnot(nrow(value) == ncol(x))
    stopifnot(!is.null(colnames(value)))
    rownames(value) <- NULL
    attr(x, 'cattr') <- value
    return(x)
}


#' @rdname cattr
#' @export
'cattr<-.zoo' <- function (x, value) {
    stopifnot(!is.null(dim(x)))    
    stopifnot(!is.null(colnames(value)))
    rownames(value) <- NULL
    attr(x, 'cattr') <- value
    class(x) <- c('zoocat', class(x))
    return(x)
} 


#' @rdname cattr
#' @export
cattr <- function (x) {
    UseMethod('cattr')
}

#' @rdname cattr
#' @export
cattr.zoocat <- function (x) {
    return(attr(x, 'cattr'))
}


