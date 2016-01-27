
#' \code{zoomly} Class
#' 
#' A class designed for monthly data.
#' 
#' \code{zoomly} class inherits \code{zoocat} class. There is one field "month"
#' in the column attributes table. 
#' 
#' @return \code{zoomly} returns a \code{zoomly} object.
#' @examples
#' 
#' x <- matrix(1 : 20, nrow = 5)
#' zm <- zoomly(x, order.by = 1991 : 1995, 
#'              colattr = data.frame(month = c(2, 3, 5, 6)))
#' 
#' @name zoomly
#' @rdname zoomly
#' @export
#' @param x a matrix or a vector. 
#' For \code{zoomly}, if x is a matrix, each row will be treated as a year. 
#' If x is a vector, it will be
#' treated as a matrix with only one column.
#' @param order.by a numeric vector representing years.
#' @param colattr a column attributes table contain a column "month".
zoomly <- function(x = NULL, order.by, colattr) {
    if (is.null(x)) {
        z <- zoo(x, frequency = 1)
        class(z) <- c('zoomly', 'zoocat', class(z))
        return(z)
    }
    if (!(is.vector(x) | is.matrix(x))) {
        stop('x must be a vector or a matrix.')
    }
    if (is.vector(x)) {
        x <- matrix(x, nrow = length(x))
    }
    if (length(order.by) != nrow(x)) {
        stop('The length of year must be equal with nrow of x.')
    }
    stopifnot('month' %in% colnames(colattr))
    
    colnames(x) <- NULL
    colattr$month <- gmon(colattr$month)
    
    zm <- zoocat(x, order.by = order.by, colattr = colattr, 
                 index.name. = 'year', frequency = 1)
    class(zm) <- c('zoomly', class(zm))
    return(zm)
}


#' @export
print.zoomly <- function (x, ...) {
    if (length(x) == 0) {
        cat('empty zoomly\n')
    } else {
        attrName <- colnames(cattr(x))
        colnames(x) <- cattr2str(attr(x, 'cattr'))
        cat('A zoomly object with:\n- [column attributes]: ')
        for (i in 1 : length(attrName)) {
            cat(attrName[i])
            if (i < length(attrName)) {
                cat(', ')
            }
        }
        cat('\n- [index variable]: ', attr(x, 'index.name'), sep = '')
        cat('\n- [data]:\n')
        class(x) <- 'zoo'
        attr(x, 'cattr') <- NULL
        attr(x, 'index.name') <- NULL
        print(x)
    }
}

