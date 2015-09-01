
#' Melt a \code{zoocat} or \code{mlydata} Object
#' 
#' Melt a \code{zoocat} object or \code{mlydata} to a data frame of the long table style,
#' which is similar as in package \code{reshape2}.
#' For \code{mlydata}, a \code{zoo} object can be returned.
#' 
#' @return A data frame or a \code{zoo} object.
#' 
#' @examples
#' 
#' x <- matrix(1 : 20, nrow = 5)
#' colAttr <- data.frame(month = c(2, 3, 5, 6), name = c('x', 'y', 'z', 'a'))
#' zc <- zoocat(x, order.by = 1991 : 1995, colattr = colAttr)
#' melt(zc)
#' 
#' @name melt
#' @rdname melt
#' @export
#' @param x A \code{zoocat} object.
#' @param value.name Name of variable used to store values.
#' @param index.name Name of variable used to store the index of the \code{zoocat} object.
melt.zoocat <- function (x, value.name = 'value', index.name = 'index') {
    colattr <- cattr(x)
    idvars <- colnames(colattr)
    xcore <- t(as.matrix(x))
    ind <- index(x)
    ind.class <- class(ind)
    dframe <- data.frame(colattr, xcore)
    colnames(dframe) <- c(idvars, ind)
    df.melt <- melt(dframe, id.vars = idvars, variable.name = index.name, value.name = value.name,
                    factorAsStrings = FALSE)
    df.melt$index <- as.character(df.melt$index)
    class(df.melt$index) <- ind.class
    df.melt <- df.melt[, c(index.name, idvars, value.name)]
    return(df.melt)
    
}



#' @export
#' @rdname melt
#' @examples
#' 
#' x <- matrix(1 : 36, nrow = 3)
#' md <- mlydata(x, year = 1991 : 1993, month = 1 : 12)
#' melt(md)
#' melt(md, ret = 'zoo')
#' 
#' @param ret Can be \code{data.frame} or \code{zoo}
melt.mlydata <- function(x, value.name = 'value', ret = 'data.frame') {
    stopifnot(ret %in% c('data.frame', 'zoo'))
    if (ret == 'zoo') {
        if(!all(attr(x, 'month') == 1 : 12)) {
            stop('x must have 12 columns which respresents 12 months.')
        }
        year <- index(x)
        month <- 1 : 12
        yy <- rep(year, each = 12)
        mm <- rep(month, length(year))
        yymm <- yearmon(yy + (mm - 1) / 12)
        mat <- coredata(x)
        vec <- as.vector(t(mat))
        ret <- zoo(vec, order.by = yymm)
    } else {
        month <- attr(x, 'month')
        year <- index(x)
        x <- data.frame(year = year, coredata(x))
        colnames(x) <- c('year', month) 
        ret <- melt(x, id.vars = 'year', variable.name = 'month',
                    value.name = value.name)
        ret$month <- as.numeric(as.character(ret$month))
    }
    return(ret)
}



