
#' Melt a \code{zoocat} Object
#' 
#' Melt a \code{zoocat} to a data frame of the long table style,
#' which is similar as in package \code{reshape2}.
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
#' zm <- as.zoomly(zc)
#' melt(zm)
#' 
#' @name melt
#' @rdname melt
#' @export
#' @param data object to melt.
#' @param value.name name of the column used to store values. It is valid only when 
#' a data frame is returned.
#' @param index.name name of the column used to store the index of the \code{zoocat} object.
#' @param na.rm as \code{melt} in reshape2. Should NA values be removed from the data set? 
#' @param ... further arguments.
melt.zoocat <- function (data, value.name = 'value', index.name = indname(data),
                         na.rm = FALSE, ...) {
    colattr <- cattr(data)
    idvars <- colnames(colattr)
    xcore <- t(as.matrix(data))
    rownames(xcore) <- NULL
    ind <- index(data)
    ind.class <- class(ind)
    dframe <- data.frame(colattr, xcore)
    colnames(dframe) <- c(idvars, as.character(ind))
    df.melt <- melt(dframe, id.vars = idvars, variable.name = index.name, value.name = value.name,
                    factorAsStrings = FALSE, na.rm = na.rm)
    df.melt[, index.name] <- as.character(df.melt[, index.name])
    coerceFunc <- match.fun(paste('as.', ind.class, sep = ''))
    df.melt[, index.name] <- coerceFunc(df.melt[, index.name])
    df.melt <- df.melt[, c(index.name, idvars, value.name)]
    return(df.melt)
}



