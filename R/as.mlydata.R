
#' Coercion Objects to Class \code{mlydata}
#' 
#' Coercion objects to class \code{mlydata}.
#' 
#' 
#' @return A mlydata object.
#' @examples
#'
#' x <-  matrix(1 : 36, nrow = 3)
#' zobj <- zoo(x, order.by = 1991 : 1993)
#' as.mlydata(zobj, month = 1 : 12)
#' 
#' @name as.mlydata
#' @rdname as.mlydata
#' @export
#' @param x A zoo object.
#' @param ... Additional arguments to be passed to or from methods.
as.mlydata <- function(x, ...) { UseMethod('as.mlydata') }

#'
#' @export
#' @rdname as.mlydata
#' @param month A vector of month, must have same length as ncol of x.
as.mlydata.zoo <- function(x, month, ...) {
    year <- index(x)
    x <- coredata(x)
    md <- mlydata(x, year = as.integer(year), month = month)
    return(md)
}


