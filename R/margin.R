#' Computing Margins
#' 
#' Computing margins for given ID variables of a data frame
#'  or given column attributes of a \code{zoocat} object. 
#'
#' @export
#' @name margin
#' @rdname margin
#' @examples 
#'
#' ## The example of data frame 
#' data(sst)
#' margin(sst, mar.for = c('year'), value.var = 'nino12')
#' margin(sst, mar.for = c('year'), value.var = 'nino3', fun.aggregate = length)
#' 
#' library(lattice)
#' sstmon <- margin(sst, mar.for = c('month'), value.var = 'nino3')
#' xyplot(nino3 ~ month, data = sstmon, type = 'o')
#' sstyr <- margin(sst, mar.for = c('year'), value.var = 'nino3')
#' xyplot(nino3 ~ year, data = sstyr, type = 'o')
#' 
#' sstmelt <- melt(sst, id.var = c('year', 'month'))
#' sstMonVar <- margin(sstmelt, mar.for = c('month', 'variable'), value.var = 'value')
#' xyplot(value~month, data = sstMonVar, groups = variable, type = 'o')
#' 
#' ## The example of zoocat 
#' zc <- cast2zoocat(sstmelt, index.var = 'year', value.var = 'value') 
#' zcmar1 <- margin(zc, mar.for = 'month')
#' zcmar2 <- margin(zc, mar.for = 'variable')
#' plot(zcmar2)
#' 
#' @param x a data frame or \code{zoocat} object.
#' @param ... additional arguments to be passed to methods.
margin <- function (x, ...) {
    UseMethod('margin')
}


#' 
#' @param mar.for one or more variables to compute margin for.
#' For data frame, it means column names. For \code{zoocat} object,
#' it means fields of column attributes.
#' @param value.var the value variable.
#' @param fun.aggregate the function for aggregation.
#' 
#' @export
#' @rdname margin
margin.data.frame <- function (x, mar.for, value.var = 'value', fun.aggregate = mean, ...) {
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
    x <- dcast(x, fm, fun.aggregate = fun.aggregate, value.var = value.var)
    colnames(x)[ncol(x)] <- value.var
    return(x)
}


#' 
#' @export
#' @rdname margin
margin.zoocat <- function (x, mar.for, fun.aggregate = mean, ...) {
    df.melt <- melt(x, value.name = 'value', index.name = 'index')
    mar.for <- cbind(mar.for, 'index')
    df.mar <- margin(df.melt, mar.for = mar.for, value.var = 'value', fun.aggregate = fun.aggregate)
    zcast <- cast2zoocat(df.mar, index.var = 'index', value.var = 'value')
    return(zcast)
}
