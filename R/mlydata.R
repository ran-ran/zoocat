
#' \code{mlydata} Class
#' 
#' A class designed for monthly data.
#' 
#' \code{mlydata} class is dependent on \code{zoo} class. An attribute
#' "month" is added for storing the month information.\cr For \code{print}
#' method, each column name is set as month abbreviation + month number. For
#' example, the column name corresponding to Feb of current year is "Feb.2".
#' When all month numbers are between 1 and 12, only the abbreviations of months is
#' printed.\cr Using \code{yr} and \code{mon} to get and set the year and the
#' month of the object.
#' 
#' @return \code{mlydata} returns a \code{mlydata} object.
#' @examples
#' 
#' x <- matrix(1 : 20, nrow = 5)
#' md <- mlydata(x, year = 1991 : 1995, month = c(2, 3, 5, 6))
#' md <- mlydata(x, order.by = 1991 : 1995, month = c(2, 3, 5, 6))
#' mon(md)
#' mon(md) <- mon(md) + 1
#' yr(md)
#' yr(md) <- yr(md) + 1
#' 
#' x <-  matrix(1 : 36, nrow = 3)
#' md <- mlydata(x, year = 1991 : 1993)
#' @name mlydata
#' @rdname mlydata
#' @export
#' @param x a matrix or a vector for function \code{mlydata}. Otherwise, it is 
#' a \code{mlydata} object. 
#' For \code{mlydata}, if x is a matrix, each row will be treated as a year. 
#' If x is a vector, it will be
#' treated as a matrix with only one column.
#' @param year a numeric vector representing years.
#' @param month a numeric vector in which numbers are used to represent months.
#' The elements of \code{month} can be smaller than 1 or larger than 12, which
#' means the months of past years or future years. For example, the months of
#' last year is (1 : 12) - 12.
#' @param order.by the same as year. If it is not NULL, argument \code{year} will
#' be negleted.
mlydata <- function(x, year, month = 1 : 12, order.by = NULL) {
    if (!(is.vector(x) | is.matrix(x))) {
        stop('x must be a vector or a matrix.')
    }
    if (is.vector(x)) {
        x <- matrix(x, nrow = length(x))
    }
    if (!is.null(order.by)) {
        year <- order.by
    }
    if (length(year) != nrow(x)) {
        stop('The length of year must be equal with nrow of x.')
    }
    if(length(month) != ncol(x)) {
        stop('The length of month must be equal with ncol of x.')
    }
    year <- as.integer(year)
    month <- as.integer(month)
    colnames(x) <- NULL
    md <- zoo(x, order.by = year, frequency = 1)
    attr(md, 'month') <- month
    class(md) <- c('mlydata', class(md))
    return(md)
}


#' @export
print.mlydata <- function(x, ...) {
    cat('A mlydata object: \n\n')
    month <- attr(x, 'month')
    z <- x
    class(z) <- 'zoo'
    attr(z, 'month') <- NULL
    colnames(z) <- month2str(month)
    print(z)
}

#' @export
#' @rdname mlydata
mon <- function(x) { UseMethod('mon') }

#' @export
#' @rdname mlydata
'mon<-' <- function(x, value) { UseMethod('mon<-') }

#' @export
#' @rdname mlydata
yr <- function(x) { UseMethod('yr') }

#' @export
#' @rdname mlydata
'yr<-' <- function(x, value) { UseMethod('yr<-') }

#' @export
#' @rdname mlydata
mon.mlydata <- function(x) {
    return(attr(x, 'month'))
}

#' @export
#' @rdname mlydata
yr.mlydata <- function(x) {
    return(index(x))
}

#' @export
#' @rdname mlydata
#' @param value The new value for month and year of the object.
'mon<-.mlydata' <- function (x, value) {
    stopifnot(length(value) == ncol(x))
    value <- as.integer(value)
    attr(x, 'month') <- value
    return(x)
}

#' @export
#' @rdname mlydata
'yr<-.mlydata' <- function (x, value) {
    stopifnot(length(value) == nrow(x))
    index(x) <- as.integer(value)
    return(x)
}



#' @export
'[.mlydata' <- function(x, i, j, drop = TRUE) {
    if (missing(i)) {
        i <- 1 : nrow(x)
    }
    if (missing(j)) {
        j <- 1 : ncol(x)
    }
    month <- attr(x, 'month')
    if (is.character(j)) {
        j <- which(j == month2str(month))
    }
    month <- month[j]
    class(x) <- class(x)[-1]
    x <- x[i, j, drop = drop]
    
    if (drop == TRUE & length(i) == 1) {
        x <- as.vector(x)
        names(x) <- month2str(month)
    } else if (drop == FALSE | (length(i) > 1 & length(j) > 1)) { 
        attr(x, 'month') <- month 
        class(x) <- c('mlydata', class(x))
    }
    
    return(x)
}
