#' Order a \code{zoocat} object by column
#' 
#' Order a "\code{zoocat}" object by column based on the values in the \code{cattr} table.
#' 
#' @examples 
#' x <- matrix(1 : 20, nrow = 5)
#' colAttr <- data.frame(month = c(2, 3, 5, 6), name = c(rep('xxx', 3), 'yyy'))
#' zm <- zoomly(x, order.by = 1991 : 1995, colattr = colAttr)
#' order_col(zm)
#' zm <- zm[, c(4, 2, 3, 1)]
#' order_col(zm)
#' 
#' @export
#' @param x the object.
order_col <- function (x) {
    stopifnot(inherits(x, 'zoocat'))
    colAttr <- cattr(x)
    if (!is.data.frame(colAttr)) {
        colAttr <- as.data.frame(colAttr)
    }
    iod <- do.call(order, args = colAttr)
    x <- x[, iod, drop = FALSE]
    return(x)
}