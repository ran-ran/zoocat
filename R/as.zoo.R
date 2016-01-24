
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
#' zc <- zoocat(x, order.by = 1991 : 1995, colattr = data.frame(month=c(2, 3, 5, 6)))
#' z <- as.zoo(zc)
#' 
#' @rdname as.zoo
#' @name as.zoo
#' @export
#' @param x an object.
#' @param add.colname logical. If TRUE, column names will be added automatically.
#' @param ... further arguments.
as.zoo.zoocat <- function (x, add.colname = TRUE, ...) {
    if (length(x) == 0){
        return(zoo())
    } else {
        if (add.colname == TRUE) {
            colnames(x) <- cattr2str(attr(x, 'cattr'))
        }
        attr(x, 'cattr') <- NULL
        attr(x, 'index.name') <- NULL
        iclass <- which(class(x) == 'zoocat') + 1
        class(x) <- class(x)[iclass : length(class(x))]
        return(x)
    }
}


