
#' Return columns with matching conditions
#' 
#' Return columns with matching conditions for the column attributes
#' table.
#' 
#' @rdname filter_col
#' @name filter_col
#' @export
#' @param x the object.
#' @param ... other arguments.
filter_col <- function (x, ...) {
    UseMethod('filter_col')
} 


#' @rdname filter_col
#' @export
#' @param cond logical predicates of conditions. Multiple conditions are 
#' combined with &.
#' @examples 
#' x <- matrix(1 : 20, nrow = 5)
#' colAttr <- data.frame(month = c(2, 3, 5, 6), name = c(rep('xxx', 3), 'yyy'))
#' zc <- zoocat(x, order.by = 1991 : 1995, colattr = colAttr)
#' filter_col(zc, month > 2)
#' temp <- 2
#' filter_col(zc, month == temp)
#' filter_col(zc, month > temp & name == 'yyy')
#' 
filter_col.zoocat <- function (x, cond) {
    colAttr <- cattr(x)
    cond_call <- substitute(cond)
    iFilt <- eval(cond_call, colAttr, parent.frame())
    ret <- x[, iFilt, drop = FALSE]
    return(ret)
} 






