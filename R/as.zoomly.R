
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
#' zc <- zoocat(x, order.by = 1991 : 1993, 
#'              colattr = data.frame(month = 1 : 12))
#' as.zoomly(zc)
#'              
#' 
#' @name as.zoomly
#' @rdname as.zoomly
#' @export
#' @param x an object.
#' @param ... further arguments passed to methods.
as.zoomly <- function(x, ...) { UseMethod('as.zoomly') }

#'
#' @export
#' @rdname as.zoomly
#' @param month a vector of month, must have same length as ncol of x.
as.zoomly.zoo <- function(x, month, ...) {
    year <- index(x)
    x <- coredata(x)
    zm <- zoomly(x, year = year, month = month)
    return(zm)
}


#' @export
#' @rdname as.zoomly
as.zoomly.zoocat <- function (x, ...) {
    if (inherits(x, 'zoomly')) {
        return(x)
    }
    colAttr <- cattr(x)
    if (ncol(colAttr) != 1 || colnames(colAttr) != 'month') {
        stop('Can not transform to zoomly object.')
    }
    class(x) <- c('zoomly', class(x))
    return(x)
}



