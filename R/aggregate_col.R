#' Aggregate "\code{zoocat}" object by column attributes
#' 
#' Aggregate columns of a "\code{zoocat}" object by given column attributes fields. 
#'
#' @export
#' @examples 
#' # This is the air quality example from package reshape2
#' names(airquality) <- tolower(names(airquality))
#' aqm <- melt(airquality, id = c("month", "day"), na.rm=TRUE) 
#' zc <- cast2zoocat(aqm, index.var = 'month', value.var = 'value', fun.aggregate = mean) 
#' aggregate_col(zc, by = 'variable', FUN = max)
#' aggregate_col(zc, by = 'variable', FUN = max, na.rm = TRUE)
#' 
#' 
#' @param x a \code{zoocat} object.
#' @param by a character string indicates the field of column attributes.
#' @param FUN a function to be applied to all data subsets.
#' @param ... additional arguments to be passed to the method.
aggregate_col <- function (x, by = colnames(cattr(x)), FUN = mean, ...) {
    if (!inherits(x, 'zoocat')) {
        stop('x must be a zoocat object.')
    }
    index.name <- indname(x)
    df.melt <- melt(x, value.name = 'value')
    str <- paste(by, collapse = '+')
    str <- paste(str, '+', index.name, sep = '')
    str <- paste('value ~', str, sep = '')
    fml <- as.formula(str)
    df.aggr <- aggregate(fml, df.melt, FUN = FUN, 
                         na.action = na.pass, ...)
    zcast <- cast2zoocat(df.aggr, index.var = index.name, value.var = 'value')
    if (inherits(zcast, 'zoocat')) {
        if (inherits(x, 'zoomly') & 'month' %in% colnames(cattr(zcast))) {
            class(zcast) <- c('zoomly', class(zcast))
        }
    }
    return(zcast)
}
