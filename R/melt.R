
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
#' @param na.rm As \code{melt} in reshape2.Should NA values be removed from the data set? 
#' For \code{mlydata}, it is only valid when \code{ret} is \code{data.frame}.
melt.zoocat <- function (x, value.name = 'value', index.name = 'index',
                         na.rm = FALSE, ...) {
    colattr <- cattr(x)
    idvars <- colnames(colattr)
    xcore <- t(as.matrix(x))
    ind <- index(x)
    ind.class <- class(ind)
    dframe <- data.frame(colattr, xcore)
    colnames(dframe) <- c(idvars, ind)
    df.melt <- melt(dframe, id.vars = idvars, variable.name = index.name, value.name = value.name,
                    factorAsStrings = FALSE, na.rm = na.rm)
    df.melt[, index.name] <- as.character(df.melt[, index.name])
    class(df.melt[, index.name]) <- ind.class
    df.melt <- df.melt[, c(index.name, idvars, value.name)]
    return(df.melt)
    
}



#' @export
#' @rdname melt
#' @examples
#' 
#' x <- matrix(1 : 36, nrow = 3, byrow = TRUE)
#' md <- mlydata(x, year = 1991 : 1993, month = 1 : 12)
#' md2 <- md + 1
#' melt(md)
#' melt(md, ret = 'zoo')
#' melt(md, md2)
#' 
#' @param ret Can be \code{data.frame} or \code{zoo}
#' @param ... Further arguments.
melt.mlydata <- function(..., value.name = 'value', variable.name = 'variable',
                         ret = 'data.frame', 
                         na.rm = FALSE) {
    stopifnot(ret %in% c('data.frame', 'zoo'))
    arg <- list(...)
    if (ret == 'zoo') {
        if (length(arg) > 1) {
            warning('For ret == "zoo", only the 1st mlydata object is used.')
        }
        x <- arg[[1]]
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
        if (length(arg) == 1) {
            x <- arg[[1]]
            month <- attr(x, 'month')
            year <- index(x)
            x <- data.frame(year = year, coredata(x))
            colnames(x) <- c('year', month) 
            ret <- melt(x, id.vars = 'year', variable.name = 'month',
                        value.name = value.name, na.rm = na.rm)
            ret$month <- as.numeric(as.character(ret$month))
            ret <- plyr::arrange(ret, year, month)
        } else {
            for (i in 2 : length(arg)) {
                if (!inherits(arg[[i]], 'mlydata')) {
                    stop('Some argument is not mlydata objects.')
                }
            }
            if (is.null(names(arg))) {
                callobj <- sys.call()
                callList <- as.list(callobj)
                argnames <- callList[2 : length(callList)]
                names(arg) <- argnames
            }
            arg <- mlydataList(arg)
            ret <- melt(arg, value.name = value.name, 
                        variable.name = variable.name, na.rm = na.rm)
        }
    }
    return(ret)
}


#' @export
#' @rdname melt
#' @examples
#' 
#' x <- matrix(1 : 20, nrow = 5)
#' x <- mlydata(x, year = 1991 : 1995, month = c(2, 3, 5, 6))
#' y <- x + 1
#' mdl <- mlydataList(x, y)
#' melt(mdl)
#' 
melt.mlydataList <- function (x, value.name = 'value', variable.name = 'variable',
                              na.rm = FALSE, ...) {
    dfmelt <- data.frame()
    varnames <- names(x)
    for (i in 1 : length(x)) {
        dfnow <- melt(x[[i]], value.name = value.name, ret = 'data.frame',
                      na.rm = na.rm)
        dfnow <- cbind(name = varnames[i], dfnow)
        colnames(dfnow)[1] <- variable.name
        dfmelt <- rbind(dfmelt, dfnow)
    }
    return(dfmelt)
}



