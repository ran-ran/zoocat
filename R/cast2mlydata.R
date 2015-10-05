
#' Cast objects to \code{zoomly} or \code{zoomlyList} Objects
#' 
#' Cast a data frame or \code{zoo} object as a \code{zoomly} or \code{zoomlyList} object.
#' 
#' If the length of \code{value.var} is larger than 1, each variable in \code{value.var}
#' will be casted as a \code{zoomly} object, and a \code{zoomlyList} will be returned.\cr
#' If the \code{variable.var} is specified, each value in \code{variable.var} corresponding 
#' to a variable, and corresponding row will be casted into a \code{zoomly} object in 
#' the returned \code{zoomlyList} object.
#' 
#' @param x a object.
#' @param ... further arguments.
#' @param year.var the name of the column which stores year.
#' @param month.var the name of the column which stores month.
#' @param value.var the name of the column which stores values. Can have 
#' several elements. NULL means all columns except \code{year.var} and \code{month.var}.
#' @param variable.var the name of the column which stores the variable names.
#' @param fun.aggregate aggregation function needed if variables do not identify a single observation
#' for each output cell. Defaults to length (with a message) if needed but not specified.
#' See \code{\link{dcast}}.
#' @return A \code{zoomly} object. 
#' @examples
#' 
#' x <- data.frame(year = rep(1991 : 1993, each = 3), month = rep(3 : 1, 3), 
#'                 value = 1 : 9)
#' zm <- cast2zoomly(x, year.var = 'year', month.var = 'month', 
#'                    value.var = 'value', fun.aggregate = mean)
#' 
#' data(sst)
#' cast2zoomly(sst, value.var = c('nino12', 'nino3'))
#' 
#' sst.melt <- melt(sst, id.var = c('year', 'month'))
#' sst.cast <- cast2zoomly(sst.melt, value.var = 'value', variable.var = 'variable')
#' sst.remelt <- melt(sst.cast)
#' 
#' ym <- as.yearmon(2000 + seq(0, 23)/12)
#' zooobj <- zoo(matrix(1:48, nrow = 24), order.by = ym)
#' colnames(zooobj) <- c('x', 'y')
#' cast2zoomly(zooobj)
#' 
#'
#' @export
#' @rdname cast2zoomly
#' @name cast2zoomly
cast2zoomly <- function (x, ...) {
    UseMethod('cast2zoomly')
}




#' @export
#' @rdname cast2zoomly
cast2zoomly.data.frame <- function (x, year.var = 'year', month.var = 'month', 
                          value.var = NULL, variable.var = NULL,
                          fun.aggregate = NULL, ...) {
    if (is.null(value.var)) {
        value.var <- setdiff(colnames(x), c(year.var, month.var))
    }
    if (length(value.var) > 1) {
        if (!is.null(variable.var)) {
            stop('variable.var must be NULL if there are more than one value.var.')
        }
    }
    if (length(value.var) == 1 & is.null(variable.var)) {
        ret <- cast2zoocat(x, index.var = year.var, value.var, attr.var = month.var,
                            fun.aggregate = fun.aggregate)
        ret <- as.zoomly(ret)
        return(ret)
    } else if (length(value.var) > 1) {
        zmList <- list()
        for (i in 1 : length(value.var)) {
            zmList[[i]] <- cast2zoomly(x, year.var = year.var, month.var = month.var,
                                        value.var = value.var[i], variable.var = NULL,
                                        fun.aggregate = fun.aggregate)
        }
        names(zmList) <- value.var
        class(zmList) <- 'zoomlyList'
        return(zmList)
    } else if (!is.null(variable.var)) {
        fml <- paste(year.var, '+', month.var, '~', variable.var, sep = '')
        xcast <- dcast(x, fml, value.var = value.var)
        value.var.new <- as.character(unique(x[, variable.var]))
        zmList <- cast2zoomly(xcast, year.var = year.var, month.var = month.var,
                               value.var = value.var.new, variable.var = NULL,
                               fun.aggregate = fun.aggregate)
        return(zmList)    
    }
    
}



#' @export
#' @rdname cast2zoomly
cast2zoomly.zoo <- function (x, value.var = NULL, fun.aggregate = NULL, ...) {
    if (is.null(value.var)) {
        value.var <- colnames(x)
    }
    yr <- as.numeric(format(index(x), '%Y'))
    mon <- as.numeric(format(index(x), '%m'))
    xdf <- data.frame(year = yr, month = mon, coredata(x),
                      stringsAsFactors = FALSE)
    ret <- cast2zoomly(xdf, year.var = 'year',
                     month.var = 'month', value.var = value.var,
                     fun.aggregate = fun.aggregate)
    return(ret)
}




