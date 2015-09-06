
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
#' coredata(md)
#' mon(md)
#' mon(md) <- mon(md) + 1
#' yr(md)
#' yr(md) <- yr(md) + 1
#' md[3, 4]
#' md[3, , drop = FALSE]
#' md[, 2]
#' md['1994', ]
#' md[, 'Apr']
#' 
#' x <-  matrix(1 : 36, nrow = 3)
#' md <- mlydata(x, year = 1991 : 1993)
#' @name mlydata
#' @rdname mlydata
#' @export
#' @param x For \code{mlydata} function, it is a matrix or a vector. If x is
#' a matrix, each row will be treated as a year. If x is a vector, it will be
#' treated as a matrix with only one column. For other methods, x is a
#' \code{mlydata} object.
#' @param year A numeric vector representing years.
#' @param month A numeric vector in which numbers are used to represent months.
#' The elements of \code{month} can be smaller than 1 or larger than 12, which
#' means the months of past years or future years. For example, the months of
#' last year is (1 : 12) - 12.
mlydata <- function(x, year, month = 1 : 12) {
    if(!(is.vector(x) | is.matrix(x))) {
        stop('x must be a vector or a matrix.')
    }
    if(is.vector(x)) {
        x <- matrix(x, nrow = length(x))
    }
    if(length(year) != nrow(x)) {
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
'[.mlydata' <- function(x, i = NULL, j = NULL, drop = TRUE) {
    if (is.null(i)) {
        i <- 1 : nrow(x)
    }
    if (is.null(j)) {
        j <- 1 : ncol(x)
    }
    month <- attr(x, 'month')
    x <- as.zoo(x)
    x <- x[i, j, drop = FALSE]    
    
    if (is.character(j)) {
        j <- which(j %in% month2str(month))
    }
    attr(x, 'month') <- month[j]
    class(x) <- c('mlydata', class(x))
    
    if (drop == TRUE) {
        if (nrow(x) == 1) {
            cname <- colnames(x)
            x <- as.vector(x)
            names(x) <- cname
        } else if (ncol(x) == 1) {
            rname <- index(x)
            x <- as.vector(x)    
            names(x) <- rname
        }
    }
    
    colnames(x) <- NULL
    return(x)
}







