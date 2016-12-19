
#' Reprocess month of \code{zoomly} objects
#' 
#' Reprocess month of "\code{zoomly}" objects, make the objects contain the data 
#' corresponding to months of previous years and following years.
#' 
#' For example, if there is a data value corresponding to year of 1990 and month of Jan, 
#' the argument \code{month} for \code{reprocess_month} can be set to be 13, and we get 
#' data of "Jan.1" (means Jan of the following year, see \link{gmon}), and the year of 
#' that data value will be 1989.
#' 
#' @examples
#' mat <- matrix(1:48, ncol = 12)
#' ctable <- data.frame(month = rep(1 : 12))
#' zm <- zoomly(mat, order.by = 1991 : 1994, colattr = ctable)
#' reprocess_month(zm, mon.repro = -11:2)
#' reprocess_month(zm, mon.repro = -24:3)
#' @param x a \code{zoomly} object.
#' @param mon.repro new setting month vector. Can be integers larger than 12 or less than 1.
#' @export
#' @return a "\code{zoomly}" object.
reprocess_month <- function (x, mon.repro) {
    if (!inherits(x, 'zoomly')) {
        stop('x must a zoomly object.')
    }
    mon.repro <- gmon(mon.repro)
    mon.true <- true_month(mon.repro)
    yr.rela <- rela_year(mon.repro)
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


