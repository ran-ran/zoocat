
#' Row Means of zoo Object
#' 
#' 
#' 
#' @param x A zoo object.
#' @param ... Additional arguments to be passed to or from methods.
#' @return A zoo object.
#' @examples
#' 
#' x <- matrix(1 : 20, nrow = 5)
#' md <- mlydata(x, year = 1991 : 1995, month = c(2, 3, 5, 6))
#' row_means(md)
#' 
#' @export
#' @name row_means
#' @rdname row_means
row_means <- function (x, ...) {
    UseMethod('row_means')
}


#' @export
#' @rdname row_means
row_means.zoo <- function (x, ...) {
    ave <- rowMeans(x)
    return(zoo(ave, order.by = index(x)))
}



