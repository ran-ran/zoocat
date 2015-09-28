
#' Coercion Objects to Class \code{zoomly}
#' 
#' Coercion objects to class \code{zoomly}.
#' 
#' 
#' @return A zoomly object.
#' @examples
#'
#' x <-  matrix(1 : 36, nrow = 3)
#' zobj <- zoo(x, order.by = 1991 : 1993)
#' as.zoomly(zobj, month = 1 : 12)
#' 
#' @name as.zoomly
#' @rdname as.zoomly
#' @export
#' @param x A zoo object.
#' @param ... further arguments passed to methods.
as.zoomly <- function(x, ...) { UseMethod('as.zoomly') }

#'
#' @export
#' @rdname as.zoomly
#' @param month a vector of month, must have same length as ncol of x.
as.zoomly.zoo <- function(x, month, ...) {
    year <- index(x)
    x <- coredata(x)
    md <- zoomly(x, year = as.integer(year), month = month)
    return(md)
}


#' @export
#' @rdname as.zoomlyas.zoomly.zoocat <- function (x) {
    colAttr <- cattr(x)
    if (!all.equal(colnames(colAttr), 'month')) {
        stop('Can not trasform to zoomly object.')
    }
    class(x) <- c('zoomly', class(x))
    return(x)
}



