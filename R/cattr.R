

#' Getting or Setting the Column Attributes of a zoocat Object.
#' 
#' Getting or setting the column attributes of a zoocat object.
#' 
#' Use attr(x, 'cattr') to get or set cattr more flexibly.
#' 
#' @usage cattr(x)
#' @examples
#' 
#' x <- matrix(1 : 20, nrow = 5)
#' colAttr <- data.frame(month = c(2, 3, 5, 6), name = 'sst')
#' zc <- zoocat(x, order.by = 1991 : 1995, colattr = colAttr)
#' colatt <- cattr(zc)
#' colatt[, 1] <- colatt[, 1] + 1
#' cattr(zc) <- colatt
#' 
#' @name cattr
#' @rdname cattr
#' @export
'cattr<-' <- function (x, value) {
    stopifnot(is.data.frame(value) | is.matrix(value))
    stopifnot(nrow(value) == ncol(x))
    stopifnot(!is.null(colnames(value)))
    rownames(value) <- NULL
    attr(x, 'cattr') <- value
    return(x)
}


#' @rdname cattr
#' @export
cattr <- function (x) {
    return(attr(x, 'cattr'))
}


