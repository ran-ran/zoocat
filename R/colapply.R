

#' Apply Functions Over Each Column
#' 
#' Apply a function over each column of the \code{zoocat} object
#' and return a data frame.
#'
#' @name colapply
#' @rdname colapply
#' @export
#' @param x A object.
#' @param ... Additional arguments to be passed to or from methods.
colapply <- function (x, ...) {
    UseMethod('colapply')
}


#' @examples
#' library(lattice)
#' 
#' data(sst)
#' sstmelt <- melt(sst, id.var = c('year', 'month'))
#' zc <- cast2zoocat(sstmelt, index.var = 'year', value.var = 'value')
#' colapply(zc, fun = function(x) {mean(x, na.rm = TRUE)}, col.as = 'vector')
#' 
#' fuse <- function (x) {
#'     mean(window(x, start = 1995, end = 2000))
#' }
#' retdf <- colapply(zc, fun = fuse, col.as = 'zoo')
#' xyplot(output~month|variable, data = retdf, type = 'o')
#' 
#' 
#' fuse <- function (x) {
#'     x <- window(x, start = 1980, end = 2014)
#'     ret <- c(max(x), min(x), mean(x), median(x), sd(x))
#'     names(ret) <- c('max', 'min', 'mean', 'median', 'sd')
#'     return(ret)
#' }
#' retdf <- colapply(zc, fun = fuse, col.as = 'zoo')
#' xyplot(sd~month|variable, data = retdf, type = 'o')
#'
#'  
#' md <- cast2mlydata(sst, year.var = 'year', value.var = 'nino12',
#'                    month.var = 'month')
#' fuse <- function(x) {
#'     xwd <- window(x, start = 1980, end = 2010)
#'     c(mean = mean(xwd), max = max(xwd), min = min(xwd))
#' }
#' colapply(md, fun = fuse, col.as = 'zoo')
#' 
#' 
#' @export
#' @rdname colapply
#' @param fun The function apply for each column.
#' @param col.as If vector, each column will be treated as a vector. If 
#' zoo, each column will be treated as a zoo object.
colapply.zoocat <- function (x, fun, col.as = 'vector', ...) {
    stopifnot(col.as %in% c('vector', 'zoo'))
    colAttr <- cattr(x)
    if (col.as == 'vector') {
        x <- coredata(x)
    } else if (col.as == 'zoo') {
        x <- as.zoo(x)
    }
    
    ret1 <- fun(x[, 1])
    if (!is.vector(ret1)) {
        ret1 <- as.vector(ret1)
    }
    outnames <- names(ret1)
    if (is.null(outnames)) {
        if (length(ret1) == 1 ) {
            outnames <- 'output'
        } else {
            outnames <- paste('output', 1 : length(ret1), sep = '.')
        }
    }
    retdf <- data.frame(colAttr, 
                        matrix(NA, nrow = nrow(colAttr), ncol = length(ret1)))
    colnames(retdf) <- c(colnames(colAttr), outnames)
    retdf[1, (ncol(colAttr) + 1) : ncol(retdf)] <- ret1
    for (i in 1 : ncol(x)) {
        vecnow <- fun(x[, i])
        if (!is.vector(vecnow)) {
            vecnow <- as.vector(vecnow)
        }
        retdf[i, (ncol(colAttr) + 1) : ncol(retdf)] <- vecnow 
    }
    return(retdf)
}


#' @export
#' @rdname colapply
colapply.mlydata <- function (x, fun, col.as = 'vector', ...) {
    stopifnot(col.as %in% c('vector', 'zoo'))
    month <- attr(x, 'month')
    if (col.as == 'vector') {
        x <- coredata(x)
    } else if (col.as == 'zoo') {
        x <- as.zoo(x)
    }
    
    ret1 <- fun(x[, 1])
    if (!is.vector(ret1)) {
        ret1 <- as.vector(ret1)
    }
    outnames <- names(ret1)
    if (is.null(outnames)) {
        if (length(ret1) == 1 ) {
            outnames <- 'output'
        } else {
            outnames <- paste('output', 1 : length(ret1), sep = '.')
        }
    }
    retdf <- data.frame(month = month, 
                        matrix(NA, nrow = length(month), ncol = length(ret1)))
    colnames(retdf) <- c('month', outnames)
    retdf[1, 2 : ncol(retdf)] <- ret1
    for (i in 1 : ncol(x)) {
        vecnow <- fun(x[, i])
        if (!is.vector(vecnow)) {
            vecnow <- as.vector(vecnow)
        }
        retdf[i, 2 : ncol(retdf)] <- vecnow 
    }
    return(retdf)
}



#' @export
#' @rdname colapply
colapply.mlydataList <- function (x, ...) {
    return(colapply(as.zoocat(x), ...))    
}




