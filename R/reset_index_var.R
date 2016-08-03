
#' Reset index variable of a \code{zoocat} object
#' 
#' @export
#' @examples 
#' 
#' x <- matrix(1 : 20, nrow = 5)
#' colAttr <- data.frame(month = c(2, 3, 5, 6), name = c(rep('xxx', 3), 'yyy'))
#' zc <- zoocat(x, order.by = 1991 : 1995, colattr = colAttr)
#' print(zc)
#' reset_index_var(zc, 'month')
#' reset_index_var(zc, 'name')
#' 
reset_index_var <- function (x, ...) {
    UseMethod('reset_index_var')
}


#' @export
reset_index_var.zoocat <- function (x, index.var) {
    if (index.var == attr(x, 'indname')) {
        return(x)
    } else {
        if (!index.var %in% names(cattr(x))) {
            stop('index.var must be one of the cattr fields.')
        }
        df.melt <- melt(x)
        ret <- cast2zoocat(df.melt, index.var = index.var, value.var = 'value')
        return(ret)
    }
}