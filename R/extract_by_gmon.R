
#' Extract data by generalized month
#' @export
#' @rdname extract_by_gmon
#' @name extract_by_gmon
extract_by_gmon <- function (x, ...) {
    UseMethod('extract_by_gmon')
}

#' @export
#' @rdname extract_by_gmon
#' @param x the object.
#' @param month numeric. A vector of integer indicates the months.
#' @examples
#' mat <- matrix(1:48, ncol = 12)
#' ctable <- data.frame(month = rep(1 : 12))
#' zm <- zoomly(mat, order.by = 1991 : 1994, colattr = ctable)
#' extract_by_gmon(zm, month = 1 : 3)
#' extract_by_gmon(zm, month = c(-9 : 8))
#' extract_by_gmon(zm, month = c(-24 : 3))
#' 
extract_by_gmon.zoomly <- function (x, month, ...) {
    stopifnot(all(month %% 1 == 0))
    month <- gmon(month)
    mon.true <- true_month(month)
    yr.rela <- rela_year(month)
    yr.rela.u <- unique(yr.rela)
    #mon.gen <- gmon(month)
         
    zm.ret <- zoomly()
    for (i in 1 : length(yr.rela.u)) {
        mon.true.now <- mon.true [yr.rela == yr.rela.u[i]]
        ret.now <- filter_col(x, cond = month %in% mon.true.now)
        if (ncol(ret.now) > 0) {
            index(ret.now) <- index(ret.now) - yr.rela.u[i]
            cattr(ret.now)$month <- 
                gmon(cattr(ret.now)$month + 12 * yr.rela.u[i])
            zm.ret <- merge(zm.ret, ret.now)
        }
    }
    
    return(zm.ret)
    
}