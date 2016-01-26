
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
filter_col_q <- function (x, ...) {
    UseMethod('filter_col_q')
} 


#' @rdname filter_col
#' @export
#' @param cond logical predicates of conditions. Multiple conditions are 
#' combined with &.
#' @examples 
#' x <- matrix(1 : 20, nrow = 5)
#' colAttr <- data.frame(month = c(2, 3, 5, 6), name = c(rep('xxx', 3), 'yyy'))
#' zc <- zoocat(x, order.by = 1991 : 1995, colattr = colAttr)
#' filter_col_q(zc, quote(month > 2))
#' temp <- 2
#' filter_col_q(zc, quote(month == temp))
#' filter_col_q(zc, quote(month > temp & name == 'yyy'))
#' 
filter_col_q.zoocat <- function (x, cond) {
    colAttr <- cattr(x) 
    iFilt <- eval(cond, colAttr, parent.frame())
    ret <- x[, iFilt, drop = FALSE]
    return(ret)
} 

#' @export
#' @examples 
#' mat <- matrix(1:48, ncol = 12)
#' ctable <- data.frame(month = rep(1 : 12))
#' zm <- zoomly(mat, order.by = 1991 : 1994, colattr = ctable)
#' filter_col_q(zm, month = 1 : 3)
#' filter_col_q(zm, month = c(-9 : 8))
#' filter_col_q(zm, month = c(-24 : 3))
filter_col_q.zoomly <- function (x, cond = NULL, month = NULL, ...) {
    if (is.null(month)) {
        if (is.null(cond)) {
            return(x)
        } else {
            return(filter_col_q.zoocat(x, cond))
        }
    }
    
    if (!all(cattr(x)$month %in% (1 : 12))) {
        stop('When using argument month, all month values in x must be in 1 : 12.')
    }
    
    if (!is.null(cond)) {
        x <- filter_col_q.zoocat(x, cond)
    }
    ret <- extract_by_month(x, month = month)
    return(ret)

}


#' @examples
#' mat <- matrix(1:48, ncol = 12)
#' ctable <- data.frame(month = rep(1 : 12))
#' zm <- zoomly(mat, order.by = 1991 : 1994, colattr = ctable)
#' extract_by_month(zm, month = -11:2)
#' extract_by_month(zm, month = -24:3)
extract_by_month <- function (x, month) {
    month <- gmon(month)
    mon.true <- true_month(month)
    yr.rela <- rela_year(month)
    yr.rela.u <- unique(yr.rela)
    
    zm.ret <- zoomly()
    for (i in 1 : length(yr.rela.u)) {
        mon.true.now <- mon.true[yr.rela == yr.rela.u[i]]
        ret.now <- x[, cattr(x)$month %in% mon.true.now]
        if (length(ret.now) > 0) {
            index(ret.now) <- index(ret.now) - yr.rela.u[i]
            cattr(ret.now)$month <- 
                gmon(cattr(ret.now)$month + 12 * yr.rela.u[i])
            zm.ret <- merge(zm.ret, ret.now)
        }
    }
    
    return(zm.ret)
}


