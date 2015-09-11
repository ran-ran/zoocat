#' mlydataList Class
#' 
#' mlydataList class.
#' 
#' 
#' @param ... A series of \code{mlydata} object.
#' The first element can be a list.
#' For this case, each elements of the list must be a \code{mlydata} object, 
#' and the list must has
#' names for each elements, and followed arguments will be neglected.
#' @return A \code{mlydataList} object.
#' @examples
#' 
#' x <- matrix(1 : 20, nrow = 5)
#' x <- mlydata(x, year = 1991 : 1995, month = c(2, 3, 5, 6))
#' y <- x + 1
#' mlydataList(list(x = x, y = y))
#' mlydataList(x, y)
#' mlydataList(x)
#' 
#' @export
mlydataList <- function(...) {
    arg <- list(...)
    stopifnot(is.list(arg[[1]]) || inherits(arg[[1]], 'mlydata'))
    if (is.list(arg[[1]])) {
        x <- arg[[1]]
        for(i in 1 : length(x)) {
            if(!(inherits(x[[i]], 'mlydata'))) {
                stop('The elements of x must be mlydata objects.')
            }
        }
        if(is.null(names(x))) {
            stop('names(x) must be specified.')
        }
        class(x) <- 'mlydataList'
        return(x)
    } else {
        if (length(arg) > 1) {
            for (i in 2 : length(arg)) {
                if (!inherits(arg[[i]], 'mlydata')) {
                    stop('Some argument is not mlydata objects.')
                }
            }
        }
        if (is.null(names(arg))) {
            callobj <- sys.call()
            callList <- as.list(callobj)
            argnames <- callList[2 : length(callList)]
            names(arg) <- argnames
        }
        class(arg) <- 'mlydataList'
        return(arg)
    }
    
}




