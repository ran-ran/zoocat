
#' Coercion Objects to Class \code{zoo}
#' 
#' Coercing objects to class \code{zoo}.
#' 
#' For \code{zoomly} and \code{zoocat}, the returned \code{zoo} object will
#' be added column names automatically. \cr
#' Note that the result of \code{as.zoo} will be a \code{zooreg} object if 
#' the input \code{x} is inherited from \code{zooreg}.
#' 
#' @return A \code{zoo} object.
#' @examples
#' 
#' x <- matrix(1 : 20, nrow = 5)
#' zm <- zoomly(x, year = 1991 : 1995, month = c(2, 3, 5, 6))
#' z <- as.zoo(zm)
#' 
#' x <- matrix(1 : 20, nrow = 5)
#' zm <- zoomly(x, year = 1991 : 1995, month = c(2, 3, 5, 6))
#' class(zm) <- class(zm)[-1]
#' as.zoo(zm)
#' 
#' @rdname as.zoo
#' @name as.zoo
#' @export
#' @param x an object.
#' @param add.colname logical. If TRUE, column names will be added automatically.
#' @param ... further arguments.
as.zoo.zoomly <- function(x, add.colname = TRUE, ...) {
    if (add.colname == TRUE) {
        colnames(x) <- month2str(mon(x))
    }
    class(x) <- class(x)[-c(1, 2)]
    attr(x, 'cattr') <- NULL
    return(x)
}


#' @rdname as.zoo
#' @export
as.zoo.zoocat <- function (x, add.colname = TRUE, ...) {
    if (length(x) == 0){
        return(zoo())
    } else {
        if (add.colname == TRUE) {
            colnames(x) <- cattr2str(attr(x, 'cattr'))
        }
        attr(x, 'cattr') <- NULL
        class(x) <- class(x)[-1]
        return(x)
    }
}


