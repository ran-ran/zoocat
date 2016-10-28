#' Translate \code{zoomly} objects to yearmon format
#' 
#' Melt the month information of a \code{zoomly} object into the index, 
#' and return a "\code{zoo}" object with the index of "\code{yearmon}" class.
#' 
#' @param x a zoomly object.
#' @export
#' @examples 
#' x <- matrix(1 : 20, nrow = 5)
#' zm <- zoomly(x, order.by = 1991 : 1995, 
#'              colattr = data.frame(month = c(2, 3, 5, 6)))
#' melt_month(zm)
#' 
#' x <- matrix(1 : 20, nrow = 5)
#' colAttr <- data.frame(month = c(2, 3, 5, 6), name = c(rep('xxx', 3), 'yyy'))
#' zc <- zoocat(x, order.by = 1991 : 1995, colattr = colAttr)
#' melt_month(as.zoomly(zc))
#' 
melt_month <- function (x) {
    df <- melt(x, value.name = 'value')
    ym <- df$year + (df$month - 1) / 12 
    ym <- as.yearmon(ym)
    
    col.yr.mon <- which(colnames(df) %in% c('year', 'month'))
    df <- df[, -1 * col.yr.mon]
    df.ym <- data.frame(yrmon = ym, df)
    
    if (ncol(df.ym) == 2) {
        ret <- zoo(df.ym[, 2], order.by = df.ym$yrmon)
    } else {
        ret <- cast2zoocat(df.ym, index.var = 'yrmon', 
                           value.var = 'value')
    }
    return(ret)
}


#' Cast month of a \code{zoomly} object
#' 
#' @param x a zoocat object.
#' @examples
#' x <- matrix(1 : 20, nrow = 5)
#' colAttr <- data.frame(month = c(2, 3, 5, 6), name = c(rep('xxx', 3), 'yyy'))
#' zc <- zoocat(x, order.by = 1991 : 1995, colattr = colAttr)
#' zmelt <- melt_month(as.zoomly(zc))
#' cast_month(zmelt)
#' 
#' @export
cast_month <- function (x) {
    df <- melt(x, value.name = 'value', index.name = 'index')
    ind <- as.Date(df$index)
    df$year <- as.numeric(format(ind, "%Y"))
    df$month <- as.numeric(format(ind, "%m"))
    df$index <- NULL
    zc <- cast2zoomly(df, index.var = 'year', value.var = 'value')
    ina <- which(apply(coredata(zc), 2, 
                       FUN = function (x) {all(is.na(x))}))
    if (length(ina) > 0)  {zc <- zc[, -ina]}
    return(zc)
}


