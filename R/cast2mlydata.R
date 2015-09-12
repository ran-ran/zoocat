
#' Cast a Data Frame as a \code{mlydata} Object
#' 
#' Cast a data frame as a \code{mlydata} object.
#' 
#' @param x A data frame.
#' @param year.var The name of the column to be treated as the index of the mlydata object.
#' @param month.var The name of the column which stores month.
#' @param value.var The name of the column which stores the values.
#' @param variable.var The name of the column which stores the variable names.
#' @param fun.aggr Aggregation function needed if variables do not identify a single observation
#' for each output cell. Defaults to length (with a message) if needed but not specified.
#' @return A \code{mlydata} object.
#' @examples
#' 
#' x <- data.frame(year = rep(1991 : 1993, each = 3), month = rep(3 : 1, 3), value = 1 : 9)
#' md <- cast2mlydata(x, index.var = 'year', month.var = 'month', value.var = 'value', fun.aggr = mean)
#' 
#' data(sst)
#' cast2mlydata(sst, value.var = c('nino12', 'nino3'))
#' 
#' sst.melt <- melt(sst, id.var = c('year', 'month'))
#' sst.cast <- cast2mlydata(sst.melt, value.var = 'value', variable.var = 'variable')
#' sst.remelt <- melt(sst.cast)
#' 
#'
#' @export
cast2mlydata <- function (x, year.var = 'year', month.var = 'month', 
                          value.var, variable.var = NULL,
                          fun.aggr = NULL) {
    if (length(value.var) > 1) {
        if (!is.null(variable.var)) {
            stop('variable.var must be NULL if there are more than one value.var.')
        }
    }
    if (length(value.var) == 1 & is.null(variable.var)) {
        zcat <- cast2zoocat(x, index.var = year.var, value.var, attr.var = month.var,
                            fun.aggr = fun.aggr)
        md <- mlydata(coredata(zcat), year = index(zcat), month = cattr(zcat)[, 1])
        return(md)
    } else if (length(value.var) > 1) {
        mdList <- list()
        for (i in 1 : length(value.var)) {
            mdList[[i]] <- cast2mlydata(x, year.var = year.var, month.var = month.var,
                                        value.var = value.var[i], variable.var = NULL,
                                        fun.aggr = fun.aggr)
        }
        names(mdList) <- value.var
        class(mdList) <- 'mlydataList'
        return(mdList)
    } else if (!is.null(variable.var)) {
        fml <- paste(year.var, '+', month.var, '~', variable.var, sep = '')
        xcast <- dcast(x, fml, value.var = value.var)
        value.var.new <- as.character(unique(x[, variable.var]))
        mdList <- cast2mlydata(xcast, year.var = year.var, month.var = month.var,
                               value.var = value.var.new, variable.var = NULL,
                               fun.aggr = fun.aggr)
        return(mdList)    
    }
    
}



