
#' Coercing Objects to Class \code{zoocat}
#' 
#' Coercing objects to class \code{zoocat}.
#' 
#' @return A \code{zoocat} object.
#' @examples
#' 
#' x <- matrix(1 : 20, nrow = 5)
#' zm <- zoomly(x, order.by = 1991 : 1995, colattr = data.frame(month = c(2, 3, 5, 6)))
#' as.zoocat(zm)
#' 
#' zobj <- zoo(matrix(1:10, nrow = 5), order.by = 11:15)
#' colnames(zobj) <- c('a', 'b')
#' as.zoocat(zobj)
#' 
#' @name as.zoocat
#' @rdname as.zoocat
#' @export
#' @param x the object.
#' @param ... other arguments passed to methods.
as.zoocat <- function (x, ...) { UseMethod('as.zoocat') }

#' @export
#' @rdname as.zoocat
as.zoocat.zoomly <- function (x, ...) {
    class(x) <- c('zoocat', 'zooreg', 'zoo')
    return(x)
}




#' @export
#' @rdname as.zoocat
#' @param colattr a data frame the column attribute table for x.
#' @param variable.name the name of the field in the \code{cattr} of 
#' the output \code{zoocat} object to store the variable name.
#' @param index.name the name of the index variable.
as.zoocat.zoo <- function (x, colattr = NULL, variable.name = 'variable', 
                           index.name = 'index', ...) {
    stopifnot(length(dim(x))== 2)
    if (is.null(colattr)) {
        stopifnot(!is.null(colnames(x)))
        colattr <- data.frame(name = colnames(x))
        colnames(colattr) <- variable.name
    }
    attr(x, 'cattr') <- colattr
    attr(x, 'indname') <- index.name
    class(x) <- c('zoocat', class(x))
    return(x)
}


