#' Order by column
#' 
#' @examples 
#' x <- matrix(1 : 20, nrow = 5)
#' colAttr <- data.frame(month = c(2, 3, 5, 6), name = c(rep('xxx', 3), 'yyy'))
#' zc <- zoocat(x, order.by = 1991 : 1995, colattr = colAttr)
#' order_col(zc)
#' zc <- zc[, c(4, 2, 3, 1)]
#' order_col(zc)
#' 
#' @export
order_col <- function (x) {
    stopifnot(inherits(x, 'zoocat'))
    colAttr <- cattr(x)
    if (!is.data.frame(colAttr)) {
        colAttr <- as.data.frame(colAttr)
    }
    iod <- do.call(order, args = colAttr)
    x <- x[, iod]
    return(x)
}