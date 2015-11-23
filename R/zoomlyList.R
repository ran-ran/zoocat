#' \code{zoomlyList} Class
#' 
#' A \code{zoomlyList} object is a list, and each element of it is 
#' a \code{zoomly} object.
#' 
#' Many methods for class \code{zoomly} are redefined for  
#' \code{zoomlyList} class. When call these methods, each element in 
#' the \code{zoomlyList} object is transfered to these methods, and 
#' the results are returned as a \code{zoomlyList} object.\cr
#' These methods include:  
#' \code{\link[=window.zoomly]{window}}, 
#' \code{\link[=expand.zoomly]{expand}}, 
#' \code{\link[=normalize.zoo]{normalize}}, 
#' \code{\link[=cummin]{cummin}}, 
#' \code{\link[=cumprod]{cumprod}}, 
#' \code{\link[=diff]{diff}}, 
#' \code{\link[=head]{head}}, 
#' \code{\link[=tail]{tail}}, 
#' \code{\link[=lag.zoomly]{lag}}, 
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
#' \code{\link[=scale]{scale}}.
#' 
#' 
#' 
#' @param ... a series of \code{zoomly} object or a list of \code{zoomly} objects.
#' These can be given as named arguments.
#' If the first argument is a list, followed arguments will be neglected.
#' Note that the list must be named.
#' @return A \code{zoomlyList} object.
#' @examples
#' 
#' x <- matrix(1 : 20, nrow = 5)
#' x <- zoomly(x, year = 1991 : 1995, month = c(2, 3, 5, 6))
#' y <- x + 1
#' zoomlyList(list(md1 = x, md2 = y))
#' zoomlyList(x, y)
#' zoomlyList(x)
#' 
#' @export
zoomlyList <- function(...) {
    arg <- list(...)
    stopifnot(is.list(arg[[1]]) || inherits(arg[[1]], 'zoomly'))
    if (is.list(arg[[1]])) {
        x <- arg[[1]]
        for(i in 1 : length(x)) {
            if(!(inherits(x[[i]], 'zoomly'))) {
                stop('The elements of x must be zoomly objects.')
            }
        }
        if(is.null(names(x))) {
            stop('names(x) must be specified.')
        }
        class(x) <- 'zoomlyList'
        return(x)
    } else {
        for (i in 1 : length(arg)) {
            if (!inherits(arg[[i]], 'zoomly')) {
                stop('Some argument is not zoomly objects.')
            }
        }
        if (is.null(names(arg))) {
            callobj <- sys.call()
            callList <- as.list(callobj)
            argnames <- callList[2 : length(callList)]
            names(arg) <- argnames
        }
        class(arg) <- 'zoomlyList'
        return(arg)
    }
    
}



#' @export
#' 
'[.zoomlyList' <- function (x, ...) {
    class(x) <- NULL
    ret <- x[...]
    class(ret) <- 'zoomlyList'
    return(ret)
}


