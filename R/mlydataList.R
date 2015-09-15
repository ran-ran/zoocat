#' \code{mlydataList} Class
#' 
#' A \code{mlydataList} object is a list, and each element of it is 
#' a \code{mlydata} object.
#' 
#' Many methods for class \code{mlydata} are redefined for  
#' \code{mlydataList} class. When call these methods, each element in 
#' the \code{mlydataList} object is transfered to these methods, and 
#' the results are returned as a \code{mlydataList} object.\cr
#' These methods include:\cr 
#' \code{\link[=window.mlydata]{window}}, 
#' \code{\link[=expand.mlydata]{expand}}, 
#' \code{\link[=normalize.zoo]{normalize}}, 
#' \code{\link[=cummin]{cummin}}, 
#' \code{\link[=cumprod]{cumprod}}, 
#' \code{\link[=diff]{diff}}, 
#' \code{\link[=head]{head}}, 
#' \code{\link[=tail]{tail}}, 
#' \code{\link[=lag.mlydata]{lag}}, 
#' \code{\link[=na.aggregate]{na.aggregate}}, 
#' \code{\link[=na.approx]{na.approx}}, 
#' \code{\link[=na.contiguous]{na.contiguous}}, 
#' \code{\link[=na.spline]{na.spline}}, 
#' \code{\link[=na.trim]{na.trim}}, 
#' \code{\link[=rollapply]{rollapply}}, 
#' \code{\link[=rollmax]{rollmax}}, 
#' \code{\link[=rollmean]{rollmean}}, 
#' \code{\link[=rollmedian]{rollmedian}}, 
#' \code{\link[=rollsum]{rollsum}}, 
#' \code{\link[=scale]{scale}}
#' 
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




