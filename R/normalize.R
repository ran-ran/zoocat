#' Normalize data
#' 
#' Normalize each column of the object using different methods. See details.
#' 
#' Three methods for normalization can be used:
#' \enumerate{
#' \item "anomaly": Each column is normalized by \eqn{x - \mu}, 
#' where \eqn{\mu} is the mean value based on the \code{base.period}.
#' \item "perc": Each column is normalized by \eqn{100 (x - \mu) / \mu}.
#' This is often called anomaly percentage.
#' \item "sd1": Each column is normalized by \eqn{(x - \mu) / \sigma}, 
#' where \eqn{\sigma} is the standard deviation based on the \code{base.period}.
#' The standard deviations of the results will be 1 if the \code{base.period} 
#' is set to be the whole time range.  
#' }
#' 
#' @examples
#' 
#' x <- matrix(1 : 20, nrow = 10)
#' colnames(x) <- c('a', 'b')
#' rownames(x) <- 1 : 10
#' normalize(x, method = 'anomaly')
#' normalize(x, method = 'perc')
#' normalize(x, method = 'sd1')
#' 
#' z <- zoo(x, order.by = 1991 : 2010)
#' normalize(z)
#' 
#' x <- matrix(1 : 20, nrow = 5)
#' colAttr <- data.frame(month = c(2, 3, 5, 6), name = c(rep('xxx', 3), 'yyy'))
#' zc <- zoocat(x, order.by = 1991 : 1995, colattr = colAttr)
#' normalize(zc)
#' 
#' @name normalize
#' @rdname normalize
#' @export
#' @param x a vector, matrix, data frame or \code{zoo} object.
#' @param ... additional arguments to be passed to or from methods.
normalize <- function (x, ...) {
    UseMethod('normalize')
}

#' @rdname normalize
#' @export
#' @param method a character string indicating which method to use. 
#' Must be 'sd1'(default), 'anomaly' or 'perc'. See details.
#' @param base.period a vector indicating the index or range of the base period.
#'  If NULL, base period is the all index range. For matrix, base.period means
#'  the row numbers.
normalize.default <- function(x, method = 'sd1', base.period = 1 : nrow(x), ...) {
    if (!(is.vector(x) | is.matrix(x) | is.data.frame(x))) {
        stop('x must be a matrix of a data frame.')
    }
    x.attr <- attributes(x)
    if (is.vector(x)) {
        x.class <- 'vector' 
        x <- matrix(x, ncol = 1)
    } else if (is.data.frame(x)) {
        x.class <- 'data.frame'
        x <- as.matrix(x)
    } else if (is.matrix(x)) {
        x.class <- 'matrix'
    } else {
        stop('unexpected class of x.')
    }
    stopifnot(length(method) == 1)
    stopifnot(any(method == c('anomaly', 'perc', 'sd1')))
    if (length(base.period) == 2) {
        base.period <- base.period[1] : base.period[2]
    }
    if (!all(base.period %in% 1 : nrow(x))){
        stop('base.period is out of range.')
    }

    xbase <- x[base.period, , drop = FALSE]
    mval <- colMeans(xbase)
    sdval <- apply(xbase, 2, FUN = sd)
    stopifnot(length(sdval) == ncol(x))
    
    vec.0 <- as.vector(t(x))
    if (method == 'anomaly') {
        vec.norm <- vec.0 - mval
    }
    if (method == 'perc') {
        vec.norm <- 100 * (vec.0 - mval) / mval
    }
    if (method == 'sd1') {
        vec.norm <- (vec.0 - mval) / sdval
    }
    
    if (x.class == 'vector') {
        ret <- vec.norm
    } else if (x.class == 'matrix') {
        ret <- matrix(vec.norm, nrow = nrow(x), byrow = TRUE)
    } else if (x.class == 'data.frame') {
        ret <- matrix(vec.norm, nrow = nrow(x), byrow = TRUE)
        ret <- as.data.frame(ret)
    }
    attributes(ret) <- x.attr
    return(ret)
}



#' @rdname normalize
#' @export
normalize.zoo <- function(x, method = 'sd1', base.period = index(x), ...) {
    if (length(base.period) == 2) {
        base.period <- index(x)[index(x) <= base.period[2] & index(x) >= base.period[1]]
    }
    stopifnot(all(base.period %in% index(x)))
    stopifnot(length(method) == 1)
    stopifnot(any(method == c('anomaly', 'perc', 'sd1')))
    
    x.attr <- attributes(x)
    ind <- index(x)
    base.period.row <- sapply(base.period, 
                              FUN = function (i) {which(i == ind)})
    ret <- normalize(coredata(x), method = method, base.period = base.period.row)
    attributes(ret) <- x.attr
    return(ret)
}
