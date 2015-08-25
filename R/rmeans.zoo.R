
#' Row Means of zoo Object
#' 
#' 
#' 
#' @usage rmeans(x)
#' @param x A zoo object.
#' @return A zoo object.
#' @examples
#' 
#' x <- matrix(1 : 20, nrow = 5)
#' md <- mlydata(x, year = 1991 : 1995, month = c(2, 3, 5, 6))
#' rmeans(md)
#' 
#' @export
#' @name rmeans
#' @rdname rmeans
rmeans <- function (x, ...) {
    UseMethod('rmeans')
}


#' @export
#' @rdname rmeans
rmeans.zoo <- function (x) {
    ave <- rowMeans(x)
    return(zoo(ave, order.by = index(x)))
}



