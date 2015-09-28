
#' Melt a \code{zoocat} or \code{zoomly} Object
#' 
#' Melt a \code{zoocat} object or \code{zoomly} to a data frame of the long table style,
#' which is similar as in package \code{reshape2}.
#' For \code{zoomly}, a \code{zoo} object can be returned.
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
#' x <- matrix(1 : 24, nrow = 3, byrow = TRUE)
#' md <- zoomly(x, year = 1991 : 1993, month = 2 : 9)
#' md2 <- md + 1
#' melt(md)
#' melt(md, ret = 'zoo')
#' melt(md, md2, ret = 'zoo')
#' 
#' x <- matrix(1 : 36, nrow = 3)
#' x <- zoomly(x, year = 1991 : 1993)
#' y <- x + 1
#' mdl <- zoomlyList(x, y)
#' melt(mdl, variable.name = 'var', value.name = 'val')
#' melt(mdl, ret = 'zoo')
#' 
#' @name melt
#' @rdname melt
#' @export
#' @param data object to melt.
#' @param value.name name of the column used to store values. It is valid only when 
#' a data frame is returned.
#' @param index.name name of the column used to store the index of the \code{zoocat} object.
#' @param na.rm as \code{melt} in reshape2. Should NA values be removed from the data set? 
#' @param ret character string. Can be \code{data.frame} or \code{zoo}
#' @param variable.name name of the column used to store variable names.
#' Only valid when a data frame is returned. For \code{melt.zoomly}, it is only 
#' valid when there are several input \code{zoomly} objects.
#' @param ... further arguments.
melt.zoocat <- function (data, value.name = 'value', index.name = 'index',
                         na.rm = FALSE, ...) {
    colattr <- cattr(data)
    idvars <- colnames(colattr)
    xcore <- t(as.matrix(data))
    ind <- index(data)
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
#' 
melt.zoomly <- function(..., value.name = 'value', variable.name = 'variable',
                         ret = 'data.frame', 
                         na.rm = FALSE) {
    stopifnot(ret %in% c('data.frame', 'zoo'))
    arg <- list(...)
    if (length(arg) > 1) {
        for (i in 2 : length(arg)) {
            if (!inherits(arg[[i]], 'zoomly')) {
                stop('Some argument is not zoomly objects.')
            }
        }
    }
    if (length(arg) == 1) {
        if (ret == 'zoo') {
            x <- arg[[1]]
            year <- index(x)
            month <- mon(x)
            yy <- rep(year, each = length(month))
            mm <- rep(month, length(year))
            yymm <- yearmon(yy + (mm - 1) / 12)
            mat <- coredata(x)
            vec <- as.vector(t(mat))
            ret <- zoo(vec, order.by = yymm)
        } else {
            x <- as.zoocat(arg[[1]])
            ret <- melt(x, index.vars = 'year', value.name = value.name, 
                        na.rm = na.rm)
            ret <- plyr::arrange(ret, year, month)
        }
    } else {
        if (is.null(names(arg))) {
            callobj <- sys.call()
            callList <- as.list(callobj)[-1]
            idxNoName <- which(nchar(names(callList)) == 0)
            argnames <- callList[idxNoName]
            names(arg) <- argnames
        }
        arg <- zoomlyList(arg)
        ret <- melt(arg, value.name = value.name, 
                    variable.name = variable.name, ret = ret, na.rm = na.rm)
    }
    return(ret)
}


#' @export
#' @rdname melt
melt.zoomlyList <- function (data, value.name = 'value', variable.name = 'variable',
                              ret = 'data.frame',
                              na.rm = FALSE, ...) {
    stopifnot(ret %in% c('data.frame', 'zoo'))
    if (ret == 'data.frame') {
        dfmelt <- data.frame()
        varnames <- names(data)
        for (i in 1 : length(data)) {
            dfnow <- melt(data[[i]], value.name = value.name, ret = 'data.frame',
                          na.rm = na.rm)
            dfnow <- cbind(name = varnames[i], dfnow)
            colnames(dfnow)[1] <- variable.name
            dfmelt <- rbind(dfmelt, dfnow)
        }
        return(dfmelt)
    } else {
        zooret <- melt(data[[1]], value.name = value.name, ret = 'zoo',
                       na.rm = na.rm)
        if (length(data) > 1) {
            for (i in 2 : length(data)) {
                zoonow <- melt(data[[i]], value.name = value.name, ret = 'zoo',
                               na.rm = na.rm)
                zooret <- cbind(zooret, zoonow)
            }
        }
        colnames(zooret) <- names(data)
        return(zooret)
    }
}



