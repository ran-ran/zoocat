
#' Detach index of \code{zoo} object
#' 
#' Detach the index of a \code{zoo} object to be seperated values of
#' year/month/day/hour/minute/second, and return a data frame containing
#' these columns.
#' 
#' @examples
#' 
#' x.Date <- as.Date("2003-02-01") + c(1, 3, 7, 9, 14) - 1
#' x <- zoo(rnorm(5), x.Date)
#' index_detach(x, 3)
#' 
#' y.POSIXct <- ISOdatetime(2003, 02, c(1, 3, 7, 9, 14), 0, 0, 0)
#' y <- zoo(rnorm(5), y.POSIXct)
#' index_detach(y, 6)
#'
#' @return a data frame.
#' @param x a object.
#' @param ... further arguments.
#' @export
#' @name index_detach
#' @rdname index_detach
index_detach <- function (x, ...) {
    UseMethod('index_detach')
}


#' @rdname index_detach
#' @export
#' @param nfield numeric. Number of fields of time to retain.
index_detach.zoo <- function (x, nfield = 2, ...) {
    stopifnot(nfield <= 6 & nfield >= 1)
    ind <- index(x)
    ind <- as.POSIXlt(ind)
    ind <- as.data.frame(unclass(ind))
    colret <- c('year', 'mon', 'mday', 'hour', 'min', 'sec')
    colret <- colret[1 : nfield]
    ind <- ind[, colret]
    cname <- c('year', 'month', 'day', 'hour', 'min', 'sec')
    colnames(ind) <- cname[1 : nfield]
    
    ind$year <- ind$year + 1900
    if ('month' %in% cname) {
        ind$month <- ind$month + 1
    }
    
    ret <- data.frame(ind, as.data.frame(x))    
    rownames(ret) <- NULL
    
    return(ret)    
    
}