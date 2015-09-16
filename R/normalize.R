#' Normalize Data
#' 
#' Normalize data or calculate anomaly or anomaly percentage.
#' 
#' @examples
#' 
#' x <- matrix(rnorm(36, 10, 4), nrow = 3)
#' normalize(x)
#' normalize(x, method = 'perc')
#' normalize(x, method = 'sd1')
#' 
#' md <- mlydata(x, year = 1991 : 1993, month = 1 : 12)
#' normalize(md)
#' normalize(md, method = 'perc')
#' normalize(md, method = 'sd1')
#' normalize(as.zoocat(md), method = 'sd1')
#' normalize(as.zoocat(md), method = 'sd1', base.period = 1991 : 1992)
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
#' "sd1" (default) means making stardard deviation to be one; 
#' "anomaly" means anomaly (departure);
#' "perc" means percentage of anomaly.
normalize.default <- function(x, method = 'sd1', ...) {
    stopifnot(length(method) == 1)
    stopifnot(any(method == c('anomaly', 'perc', 'sd1')))

    like.vec <- FALSE
    if (is.null(dim(x))) {
        x <- matrix(x, ncol = 1)
        like.vec <- TRUE
    }
    xa <- x
    if (method == 'anomaly') {
        for(i in 1 : ncol(x)) {
            xa[, i] <- x[, i] - mean(x[, i], na.rm = TRUE)
        }
    }
    if (method == 'perc') {
        for (i in 1 : ncol(x)) {
            xa[, i] <- 100 * (x[, i] - mean(x[, i], na.rm = TRUE)) / mean(x[, i], na.rm = TRUE)
        }
    }
    if (method == 'sd1') {
        for (i in 1 : ncol(x)) {
            xa[, i] <- (x[, i] - mean(x[, i], na.rm = TRUE)) / sd(x[, i], na.rm = TRUE)
        }
    }
    if (like.vec == TRUE) {
        dim(xa) <- NULL
    }

    return(xa)
}



#' @rdname normalize
#' @export
#' @param base.period a vector. If NULL, base period is the all index range.
normalize.zoo <- function(x, method = 'sd1', base.period = NULL, ...) {
    if (is.null(base.period)) {
        base.period <- index(x)
    }
    stopifnot(all(base.period %in% index(x)))
    stopifnot(length(method) == 1)
    stopifnot(any(method == c('anomaly', 'perc', 'sd1')))
    
    like.vec <- FALSE
    if (is.null(dim(x))) {
        x <- zoo(matrix(coredata(x), ncol = 1), order.by = index(x))
        like.vec <- TRUE
    }
    xa <- x
    if (method == 'anomaly') {
        for(i in 1 : ncol(x)) {
            sbase <- window(x, index. = base.period)[, i]
            xa[, i] <- x[, i] - mean(sbase, na.rm = TRUE)
        }
    }
    if (method == 'perc') {
        for (i in 1 : ncol(x)) {
            sbase <- window(x, index. = base.period)[, i]
            xa[, i] <- 100 * (x[, i] - mean(sbase, na.rm = TRUE)) / mean(sbase, na.rm = TRUE)
        }
    }
    if (method == 'sd1') {
        for (i in 1 : ncol(x)) {
            sbase <- window(x, index. = base.period)[, i]
            xa[, i] <- (x[, i] - mean(sbase, na.rm = TRUE)) / sd(sbase, na.rm = TRUE)
        }
    }
    if (like.vec == TRUE) {
        dim(xa) <- NULL
    }

    return(xa)
}
