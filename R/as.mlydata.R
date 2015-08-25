
#' Coercion Objects to Class \code{mlydata}
#' 
#' Coercion objects to class \code{mlydata}.
#' 
#' 
#' @param x A zoo object.
#' @param month A vector of month, must have same length as ncol of x.
#' @return A mlydata object.
#' @examples
#'
#' x <-  matrix(1 : 36, nrow = 3)
#' zobj <- zoo(x, order.by = 1991 : 1993)
#' md <- as.mlydata(zobj, month = 1:12)
#' 
#' x <-  matrix(1 : 36, nrow = 3)
#' zcat <- zoocat(x, order.by = 1991 : 1993, colattr = data.frame(month = 1 : 12))
#' md <- as.mlydata(zcat)
#' 
#' @name as.mlydata
#' @rdname as.mlydata
#' @export
as.mlydata <- function(x, ...) { UseMethod('as.mlydata') }

#'
#' @export
#' @rdname as.mlydata
as.mlydata.zoo <- function(x, month = 1 : 12) {
    year <- index(x)
    x <- coredata(x)
    md <- mlydata(x, year = year, month = month)
    return(md)
}


#'
#' @export
#' @rdname as.mlydata
as.mlydata.zoocat <- function (x) {
    if (ncol(cattr(x)) != 1) {
        stop('x must only have one column attribute.')
    }
    year <- index(x)
    month <- as.vector(cattr(x)[, 1])
    md <- mlydata(coredata(x), year = year, month = month)
    return(md)
}





