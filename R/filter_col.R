
#' Return columns with matching conditions
#' 
#' Return columns with matching conditions for the column attributes (\bold{cattr}) table.
#' 
#' For \code{filter_col}, when the argument \code{mon.repro} is not \code{NULL}, \code{reprocess_month}
#' will be called in the last step.
#' 
#' @rdname filter_col
#' @name filter_col
#' @export
#' @return a "\code{zoocat}" or "\code{zoomly}" object.
#' @param x the object.
#' @param cond logical predicates of conditions. Multiple conditions are 
#' combined with \code{&}. 
#' For \code{filter_col}, \code{cond} must be an expression, 
#' while for \code{filter_col_q}, \code{cond} must be a quoted expression.
#' @param mon.repro the reprocessing month vector, which is used for \code{\link{reprocess_month}}. 
#' See details.
#' 
#' 
#' @param ... other arguments.
#' @examples 
#' x <- matrix(1 : 20, nrow = 5)
#' colAttr <- data.frame(month = c(2, 3, 5, 6), name = c(rep('xxx', 3), 'yyy'))
#' zc <- zoocat(x, order.by = 1991 : 1995, colattr = colAttr)
#' filter_col(zc, month > 2)
#' filter_col(zc, month > 2)
#' filter_col(zc, month > 2 & name == 'yyy')
#' 
#' mat <- matrix(1:48, ncol = 12)
#' colAttr <- data.frame(month = rep(1 : 12))
#' zm <- zoomly(mat, order.by = 1991 : 1994, colattr = colAttr)
#' filter_col(zm, mon.repro = 1 : 3)
#' filter_col(zm, mon.repro = c(-9 : 8))
#' filter_col(zm, cond = month %in% 1 : 3, mon.repro = c(-24 : 3))
#' 
filter_col_q <- function (x, ...) {
    UseMethod('filter_col_q')
} 

#' @rdname filter_col
#' @export
filter_col <- function (x, ...) {
    UseMethod('filter_col')
} 

#' @rdname filter_col
#' @export
filter_col_q.zoocat <- function (x, cond, ...) {
    if (missing(cond)) {
        return(x)
    }
    
    colAttr <- cattr(x) 
    iFilt <- eval(cond, colAttr, parent.frame())
    ret <- x[, iFilt, drop = FALSE]
    return(ret)
} 


#' @export
#' @rdname filter_col
filter_col.zoocat <- function (x, cond, ...) {
    if (missing(cond)) {
        return(x)
    }
    
    cond <- substitute(cond)
    colAttr <- cattr(x) 
    iFilt <- eval(cond, colAttr, parent.frame())
    ret <- x[, iFilt, drop = FALSE]
    return(ret)
} 


#' @export
#' @rdname filter_col
filter_col_q.zoomly <- function (x, cond, mon.repro = NULL, ...) {
    if (!missing(cond)) {
        colAttr <- cattr(x) 
        iFilt <- eval(cond, colAttr, parent.frame())
        x <- x[, iFilt, drop = FALSE]
    }
    
    if (!is.null(mon.repro)) {
        if (!all(cattr(x)$month %in% (1 : 12))) {
            stop('When using argument mon.repro, all month values in x must be in 1 : 12.')
        }
        x <- reprocess_month(x, mon.repro = mon.repro)
    }
    
    return(x)
}


#' @export
#' @rdname filter_col
filter_col.zoomly <- function (x, cond, mon.repro = NULL, ...) {
    if (!missing(cond)) {
        cond <- substitute(cond)
        colAttr <- cattr(x) 
        iFilt <- eval(cond, colAttr, parent.frame())
        x <- x[, iFilt, drop = FALSE]
    }
    
    if (!is.null(mon.repro)) {
        if (!all(cattr(x)$month %in% (1 : 12))) {
            stop('When using argument mon.repro, all month values in x must be in 1 : 12.')
        }
        x <- reprocess_month(x, mon.repro = mon.repro)
    }
    return(x)
}

