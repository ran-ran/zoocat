#' Normalize Data
#' 
#' @examples
#' 
#' x <- matrix(rnorm(36, 10, 4), nrow = 3)
#' normalize(x)
#' normalize(x, mode = 'perc')
#' normalize(x, mode = 'sd1')
#' 
#' md <- mlydata(x, year = 1991 : 1993, month = 1 : 12)
#' normalize(md)
#' normalize(md, mode = 'perc')
#' normalize(md, mode = 'sd1')
#' normalize(as.zoocat(md), mode = 'sd1')
#' normalize(as.zoocat(md), mode = 'sd1', base.period = 1991 : 1992)
#' 
#' @name normalize
#' @rdname normalize
#' @export
#' @param x A vector, matrix, data frame or zoo object.
#' @param ... Additional arguments to be passed to or from methods.
normalize <- function (x, ...) {
    UseMethod('normalize')
}

#' @rdname normalize
#' @export
#' @param mode \code{perc} means percentage of anomaly. \code{sd1} means
#' normalized anomaly.
normalize.default <- function(x, mode = 'anomaly', ...) {
    stopifnot(length(mode) == 1)
    stopifnot(any(mode == c('anomaly', 'perc', 'sd1')))

    like.vec <- FALSE
    if (is.null(dim(x))) {
        x <- matrix(x, ncol = 1)
        like.vec <- TRUE
    }
    xa <- x
    if (mode == 'anomaly') {
        for(i in 1 : ncol(x)) {
            xa[, i] <- x[, i] - mean(x[, i], na.rm = TRUE)
        }
    }
    if (mode == 'perc') {
        for (i in 1 : ncol(x)) {
            xa[, i] <- 100 * (x[, i] - mean(x[, i], na.rm = TRUE)) / mean(x[, i], na.rm = TRUE)
        }
    }
    if (mode == 'sd1') {
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
#' @param base.period If NULL, base period is the all index range.
normalize.zoo <- function(x, mode = 'anomaly', base.period = NULL, ...) {
    if (is.null(base.period)) {
        base.period <- index(x)
    }
    stopifnot(all(base.period %in% index(x)))
    stopifnot(length(mode) == 1)
    stopifnot(any(mode == c('anomaly', 'perc', 'sd1')))
    
    like.vec <- FALSE
    if (is.null(dim(x))) {
        x <- zoo(matrix(coredata(x), ncol = 1), order.by = index(x))
        like.vec <- TRUE
    }
    xa <- x
    if (mode == 'anomaly') {
        for(i in 1 : ncol(x)) {
            sbase <- window(x, index. = base.period)[, i]
            xa[, i] <- x[, i] - mean(sbase, na.rm = TRUE)
        }
    }
    if (mode == 'perc') {
        for (i in 1 : ncol(x)) {
            sbase <- window(x, index. = base.period)[, i]
            xa[, i] <- 100 * (x[, i] - mean(sbase, na.rm = TRUE)) / mean(sbase, na.rm = TRUE)
        }
    }
    if (mode == 'sd1') {
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
