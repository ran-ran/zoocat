#' Cast a Data Frame as a zoocat Object
#' 
#' Cast a data frame into a zoocat object. The data frame should be the style
#' of the molten data frame in package reshape2.
#' 
#' 
#' @param x a data frame.
#' @param index.var the name of the column to be treated as the index of the
#' zoocat object.
#' @param value.var the name of the column which stored the values.
#' @param attr.var the name of the column which will be used as column attributes of 
#' the \code{zoocat} object. If NULL, all columns except \code{value.var} and \code{index.var}
#' will be used.
#' @param fun.aggregate aggregation function needed if variables do not identify a single observation
#' for each output cell. Defaults to length (with a message) if needed but not specified.
#' See \code{\link{dcast}}.
#' @param del.unique.cattr logical. If TRUE, the column attibutes with unique value will
#' be deleted.
#' @return A \code{zoocat} object.
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

    stopifnot(is.data.frame(x))
    stopifnot(!is.null(colnames(x)))
    stopifnot(length(index.var) == 1)
    stopifnot(length(value.var) == 1)
    stopifnot(index.var != value.var)
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
    
    index.class <- class(x[,  index.var])
    if (is.factor(x[, index.var])) {
        index.levels <- levels(x[, index.var])
    }
    
    left <- paste(attr.var, collapse = "+")
    right <- paste(index.var, collapse = "+")
    fm <- paste(left, right, sep = "~")
    data.cast <- dcast(x, fm, fun.aggregate = fun.aggregate, value.var = value.var)

    mat <- as.matrix(data.cast[, (nattr + 1) : ncol(data.cast), drop = FALSE])
    if (index.class != 'factor') {
        ord <- as(colnames(mat), index.class)
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
        zcat <- zoocat(t(mat), order.by = ord, colattr = col.attr)
    }
    return(zcat)
}



