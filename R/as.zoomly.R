
#' Coercion objects to class \code{zoomly}
#' 
#' Coercion objects to class "\code{zoomly}". The index name of the object will be set to "year".
#' 
#' 
#' @return A "\code{zoomly}" object.
#' @examples
#' x <-  matrix(1 : 36, nrow = 3)
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



#' @export
#' @rdname as.zoomly
#' @examples 
#' x <- matrix(1 : 20, nrow = 5)
#' colAttr <- data.frame(month = c(2, 3, 5, 6), name = c(rep('xx', 3), 'yy'))
#' zc <- zoocat(x, order.by = 1991 : 1995, colattr = colAttr)
#' as.zoomly(zc)
#' 
as.zoomly.zoocat <- function (x, ...) {
    if (inherits(x, 'zoomly')) {
        return(x)
    }
    if (! 'month' %in% colnames(cattr(x))) {
        stop('month must be one of cattr fields.')
    }
    if (any(index(x) != round(index(x)))) {
        stop('index must be integers.')
    }
    attr(x,'frequency') <- 1
    attr(x, 'indname') <- 'year'
    cattr(x)$month <- gmon(cattr(x)$month)
    index(x) <- as.numeric(index(x))
    class(x) <- c('zoomly', 'zoocat', 'zooreg', 'zoo')
    return(x)
}



