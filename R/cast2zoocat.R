#' Cast a data frame to a \code{zoocat} object
#' 
#' Cast a data frame to a "\code{zoocat}" object. The data frame should be 
#' a long format data frame (for example, melted by \code{reshape2::melt}).
#' 
#' 
#' @param x a data frame.
#' @param index.var the name of the column to be treated as the index of the
#' \code{zoocat} object.
#' @param value.var the name of the column which stored the values.
#' @param attr.var the name of the column which will be used as column attributes of 
#' the \code{zoocat} object. If NULL, all columns except \code{value.var} and \code{index.var}
#' will be used.
#' @param fun.aggregate aggregation function needed if variables do not identify a single observation
#' for each output cell. Defaults to length (with a message) if needed but not specified.
#' See \code{\link{dcast}}.
#' @param del.unique.cattr logical. If TRUE, the column attibutes with unique value will
#' be deleted.
#' @return a "\code{zoocat}" object.
#' @examples
#' 
#' df <- data.frame(year = rep(1991 : 1995, each = 24), month = rep(1 : 12, 10),
#'                  varname = rep(c('a', 'b'), each = 12), city = rep(1 : 3, each = 40),
#'                  value = 1 : 120)
#' cast2zoocat(df, index.var = 'year', value.var = 'value')
#' cast2zoocat(df, index.var = 'year', value.var = 'value', attr.var = 'varname')
#' 
#' ## This is the air quality example from package reshape2
#' names(airquality) <- tolower(names(airquality))
#' aqm <- melt(airquality, id = c("month", "day"), na.rm=TRUE) 
#' head(aqm)
#' cast2zoocat(aqm, index.var = 'month', value.var = 'value', attr.var = 'variable')
#' cast2zoocat(aqm, index.var = 'month', value.var = 'value')
#' 
#' 
#' @export
cast2zoocat <- function (x, index.var, value.var, attr.var = NULL, fun.aggregate = NULL,
                         del.unique.cattr = TRUE) {

    if (!is.data.frame(x)) {
        stop('x must ba a data frame.')
    }
    x <- as.data.frame(x)
    if (is.null(colnames(x))) {
        stop('x must have column names.')
    }
    if (length(index.var) > 1) {
        stop('There must be only one index variable.')
    }
    if (length(value.var) > 1) {
        stop('There must be only one value variable.')
    }
    if (index.var == value.var) {
        stop('index.var and value.var can not be the same.')
    }
    varnames <- colnames(x)
    if (!index.var %in% varnames) {
        stop('index.var does not exist in x.')
    }
    if (!value.var %in% varnames) {
        stop('value.var does not exist in x.')
    }
    if (is.null(attr.var)) {
        attr.var <- setdiff(varnames, c(index.var, value.var))
    }
    nattr <- length(attr.var)
    if (nattr == 0) {
        cat('There is no column attribute, so a zoo object is returned.\n')
        return(zoo(x$value.var, order.by = x$index.var))
    } else {
        if (!all(attr.var %in% varnames)) {
            stop('Some attr.var does not exist in x.')
        }
    }
    
    index.class <- class(x[1,  index.var])
    if (is.factor(x[, index.var])) {
        index.levels <- levels(x[, index.var])
    }
    x[, index.var] <- as.character(x[, index.var])
    
    left <- paste(attr.var, collapse = "+")
    right <- paste(index.var, collapse = "+")
    fm <- paste(left, right, sep = "~")
    data.cast <- dcast(x, fm, fun.aggregate = fun.aggregate, value.var = value.var)

    mat <- as.matrix(data.cast[, (nattr + 1) : ncol(data.cast), drop = FALSE])
    if (index.class != 'factor') {
        coerce.func <- paste('as.', index.class, sep = '')
        ord <- match.fun(coerce.func)(colnames(mat))
    } else {
        ord <- factor(colnames(mat), levels = index.levels)
    }
    col.attr <- data.cast[, 1 : nattr, drop = FALSE]
    
    if (del.unique.cattr == TRUE) {
        iDel <- c()
        for (i in 1 : ncol(col.attr)) {
            if (all(col.attr[, i] == col.attr[1, i])) {
                cat('Delete the column attribute "', colnames(col.attr)[i],
                    '" for the unique value "', col.attr[1, i], '".\n', sep = '')
                iDel <- c(iDel, i)
            }
        }
        if (length(iDel) > 0) {
            col.attr <- col.attr[, -iDel, drop = FALSE]
        }
    }
    
    if (ncol(col.attr) == 0) {
        zcat <- zoo(t(mat), order.by = ord)
        cat('There is no column attribute, so a zoo object is returned.\n')
    } else {
        zcat <- zoocat(t(mat), order.by = ord, colattr = col.attr,
                       index.name = index.var)
    }
    return(zcat)
}



#' Cast a data frame to a \code{zoomly} object
#' 
#' Cast a data frame to a "\code{zoomly}" object.
#' 
#' @return a "\code{zoomly}" object.
#' 
#' @export
#' @param x a data frame containing a column named "month".
#' @param ... other arguments for cast2zoocat.
#' @examples
#' df <- data.frame(year = rep(1991 : 1995, each = 24), month = rep(1 : 12, 10),
#'                  varname = rep(c('a', 'b'), each = 12), city = rep(1 : 3, each = 40),
#'                  value = 1 : 120)
#' cast2zoomly(df, index.var = 'year', value.var = 'value')
#' 
cast2zoomly <- function (x, ...) {
    zc <- cast2zoocat(x, ...)
    return(as.zoomly(zc))
}

