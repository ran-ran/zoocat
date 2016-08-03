
#' Return columns with matching conditions
#' 
#' Return columns with matching conditions for the column attributes
#' table.
#' 
#' @rdname filter_col
#' @name filter_col
#' @export
#' @param x the object.
#' @param cond logical predicates of conditions. Multiple conditions are 
#' combined with &.
#' @param mon.repro the month to extract from 
#' the \code{zoomly} object for reprocessing month. See details. 
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
    colAttr <- cattr(x) 
    iFilt <- eval(cond, colAttr, parent.frame())
    ret <- x[, iFilt, drop = FALSE]
    return(ret)
} 


#' @export
#' @rdname filter_col
filter_col.zoocat <- function (x, cond, ...) {
    cond_call <- substitute(cond)
    return(filter_col_q(x, cond_call))
} 


#' @export
#' @rdname filter_col
filter_col_q.zoomly <- function (x, cond = NULL, mon.repro = NULL, ...) {
    if (is.null(mon.repro)) {
        if (is.null(cond)) {
            return(x)
        } else {
            return(filter_col_q.zoocat(x, cond))
        }
    }
    
    if (!all(cattr(x)$month %in% (1 : 12))) {
        stop('When using argument mon.repro, all month values in x must be in 1 : 12.')
    }
    
    if (!is.null(cond)) {
        x <- filter_col_q.zoocat(x, cond)
    }
    ret <- reprocess_month(x, month = mon.repro)
    return(ret)
}


#' @export
#' @rdname filter_col
filter_col.zoomly <- function (x, cond = NULL, mon.repro = NULL, ...) {
    cond_call <- substitute(cond)
    return(filter_col_q(x, cond_call, mon.repro = mon.repro))
}


#' Reprocess month of zoomly
#' @examples
#' mat <- matrix(1:48, ncol = 12)
#' ctable <- data.frame(month = rep(1 : 12))
#' zm <- zoomly(mat, order.by = 1991 : 1994, colattr = ctable)
#' reprocess_month(zm, month = -11:2)
#' reprocess_month(zm, month = -24:3)
#' @param x a \code{zoomly} object.
#' @param month new setting month vector.
reprocess_month <- function (x, month) {
    month <- gmon(month)
    mon.true <- true_month(month)
    yr.rela <- rela_year(month)
    yr.rela.u <- unique(yr.rela)
    
    zm.ret <- zoomly()
    for (i in 1 : length(yr.rela.u)) {
        mon.true.now <- mon.true[yr.rela == yr.rela.u[i]]
        ret.now <- x[, cattr(x)$month %in% mon.true.now, drop = FALSE]
        if (length(ret.now) > 0) {
            index(ret.now) <- index(ret.now) - yr.rela.u[i]
            cattr(ret.now)$month <- 
                gmon(cattr(ret.now)$month + 12 * yr.rela.u[i])
            zm.ret <- merge(zm.ret, ret.now)
        }
    }
    attr(zm.ret, 'indname') <- attr(x, 'indname')
    zm.ret <- order_col(zm.ret)
    return(zm.ret)
}


