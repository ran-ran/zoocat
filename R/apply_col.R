

#' Apply Functions Over Each Column
#' 
#' Apply a function over each column of the \code{zoocat} object
#' and return a data frame.
#'
#' @name apply_col
#' @rdname apply_col
#' @export
#' @param x A object.
#' @param ... Additional arguments to be passed to or from methods.
apply_col <- function (x, ...) {
    UseMethod('apply_col')
}


#' @examples
#' library(lattice)
#' 
#' data(nino)
#' ninomelt <- melt(nino, id.var = c('year', 'month'))
#' zc <- cast2zoocat(ninomelt, index.var = 'year', value.var = 'value')
#' apply_col(zc, fun = function(x) {mean(x, na.rm = TRUE)}, col.as = 'vector')
#' 
#' fuse <- function (x) {
#'     mean(window(x, start = 1995, end = 2000))
#' }
#' retdf <- apply_col(zc, fun = fuse, col.as = 'zoo')
#' xyplot(output~month|variable, data = retdf, type = 'o')
#' 
#' 
#' fuse <- function (x) {
#'     x <- window(x, start = 1980, end = 2014)
#'     ret <- c(max(x), min(x), mean(x), median(x), sd(x))
#'     names(ret) <- c('max', 'min', 'mean', 'median', 'sd')
#'     return(ret)
#' }
#' retdf <- apply_col(zc, fun = fuse, col.as = 'zoo')
#' xyplot(sd~month|variable, data = retdf, type = 'o')
#'  
#' @export
#' @rdname apply_col
#' @param fun The function apply for each column.
#' @param col.as If vector, each column will be treated as a vector. If 
#' zoo, each column will be treated as a zoo object.
apply_col.zoocat <- function (x, fun, col.as = 'vector', ...) {
    if (! (col.as %in% c('vector', 'zoo'))) {
        stop('col.as must be \'vector\' or \'zoo\'.')
    }
    colAttr <- cattr(x)
    if (col.as == 'vector') {
        x <- as.matrix(x)
    } else if (col.as == 'zoo') {
        x <- as.zoo(x, add.colname = FALSE)
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
    outmat <- matrix(NA, nrow = nrow(colAttr), ncol = length(ret1))
    colnames(outmat) <- outnames
    outmat[1, ] <- ret1
    for (i in 2 : ncol(x)) {
        vecnow <- fun(x[, i])
        if (!is.vector(vecnow)) {
            vecnow <- as.vector(vecnow)
        }
        outmat[i, ] <- vecnow 
    }
    ret <- data.frame(colAttr, outmat)
    return(ret)
}





