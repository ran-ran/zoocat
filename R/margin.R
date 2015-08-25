#' Computing Margins
#' 
#' Computing margins for given ID variables of a data frame
#'  or given column attributes of a \code{zoocat} object. 
#'
#' @param x A data frame or \code{zoocat} object.
#' @export
#' @name margin
#' @rdname margin
#' @examples 
#'
#' ## The example of data frame 
#' lat <- 1:5
#' lon <- 101:105
#' time <- 1991:2000
#' data <- data.frame(lat = rep(lat, 50), lon = rep(rep(lon, each = 5), 10),
#'                    time = rep(time, each = 25), value = 1 : 250)
#' agg1 <- margin(data, mar.for = c('lat'))
#' agg2 <- margin(data, mar.for = c('lon', 'lat'))
#' agg3 <- margin(data, mar.for = c('lon', 'lat'), fun.aggr = sum)
#' agg4 <- margin(data, mar.for = c('lon'), fun.aggr = sum)
#' 
#' ## The example of zoocat 
#' x <- matrix(1 : 20, nrow = 5)
#' colAttr <- data.frame(month = c(2, 3, 5, 6), name = c('a', 'b'))
#' zc <- zoocat(x, order.by = 1991 : 1995, colattr = colAttr)
#' zc.mar <- margin(zc, mar.for = 'month')
#' 
#' 
margin <- function (x, ...) {
    UseMethod('margin')
}


#' 
#' @param mar.for One or more variables to compute margin for.
#' For data frame, it means column names. For \code{zoocat} object,
#' it means names of column attributes.
#' @param value.var The value variable.
#' @param fun.aggr The function for aggregation.
#' 
#' @export
#' @rdname margin
margin.data.frame <- function (x, mar.for, value.var = 'value', fun.aggr = mean) {
    stopifnot(is.data.frame(x))
    nvar <- ncol(x)
    cnames <- colnames(x)
    if (!all(value.var %in% cnames)) {
        stop('value.var does not exist in data.')
    }    
    if (!all(mar.for %in% cnames)) {
        stop('mar.for does not exist in data')
    }
    stopifnot(length(value.var) == 1)
    if (value.var %in% mar.for) {
        stop('value.var should not be one of mar.for')
    }
    
    x <- cbind(x, cons = 1)
    left.var <- mar.for
    right.var <- 'cons'
    left <- paste(left.var, collapse = '+')
    right <- paste(right.var, collapse = '+')
    fm <- paste(left, right, sep = '~')
    x <- dcast(x, fm, fun.aggregate = fun.aggr, value.var = value.var)
    colnames(x)[ncol(x)] <- value.var
    return(x)
}


#' 
#' @export
#' @rdname margin
margin.zoocat <- function (x, mar.for, fun.aggr = mean) {
    df.melt <- melt(x, value.name = 'value', index.name = 'index')
    mar.for <- cbind(mar.for, 'index')
    df.mar <- margin(df.melt, mar.for = mar.for, value.var = 'value', fun.aggr = fun.aggr)
    zcast <- cast2zoocat(df.mar, index.var = 'index', value.var = 'value')
    return(zcast)
}
