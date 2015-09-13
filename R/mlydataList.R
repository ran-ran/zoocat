#' \code{mlydataList} Class
#' 
#' A \code{mlydataList} object is a list, each element of which is 
#' a \code{mlydata} object.
#' 
#' 
#' @param ... a series of \code{mlydata} object or a list of \code{mlydata} objects.
#' These can be given as named arguments.
#' If the first argument is a list, followed arguments will be neglected.
#' Note that the list must be named.
#' @return A \code{mlydataList} object.
#' @examples
#' 
#' x <- matrix(1 : 20, nrow = 5)
#' x <- mlydata(x, year = 1991 : 1995, month = c(2, 3, 5, 6))
#' y <- x + 1
#' mlydataList(list(md1 = x, md2 = y))
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
        for (i in 1 : length(arg)) {
            if (!inherits(arg[[i]], 'mlydata')) {
                stop('Some argument is not mlydata objects.')
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




