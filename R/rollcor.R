#' Calculate the Rolling Window Correlation
#' 
#' 
#' 
#' @usage rollcor(x, y, width, show = TRUE, ...)
#' @param x,y Two vectors or two zoo objects. For zoo objects, if their time
#' ranges is different, intersection will be used.
#' @param width The width of the sliding window, which must be odd number.
#' @param show If TRUE, the result will be plotted.
#' @param ...  Other arguments for function cor.
#' @examples
#' 
#' x <- 1 : 100
#' y <- 2 * x + rnorm(100, 0, 10)
#' rollcor(x, y, width = 21)
#' 
#' xz <- zoo(x)
#' yz <- zoo(y)
#' rollcor(xz, yz, width = 21)
#' 
#' rollcor(xz, yz, width = 21, show = FALSE)
#' 
#' x <- 1 : 100
#' y <- 2 * x  + rnorm(100, 0, 10)
#' x <- zoo(x, order.by = 10 : 109)
#' y <- zoo(y, order.by = -3 : 96)
#' rollcor(x, y, width = 21, method = 'kendall')
#' 
#' @export
#' @name rollcor
#' @rdname rollcor
rollcor <- function (x, ...) {
    UseMethod('rollcor')
}



#' @export
#' @rdname rollcor
rollcor.default <- function (x, y, width, show = TRUE, ...) {
    x <- as.vector(x)
    y <- as.vector(y)
    stopifnot(length(x) == length(y))
    stopifnot((width %% 2) != 0)

    len <- length(x)
    halfWidth <- (width - 1) / 2
    lenCC <- len - 2 * halfWidth
    cc <- rep(0, lenCC)
    names(cc) <- (halfWidth + 1) : (len - halfWidth)
    for (i in 1 : lenCC) {
        start <- i
        end <- start + width - 1
        cc[i] <- cor(coredata(x)[start : end], coredata(y)[start : end], ...)
    }
    if (show == TRUE) {
        dev.new()
        plot(cc, type = 'o', main = 'Sliding Correlations', xlab = 'Time', ylab = '')
        return(invisible(cc))
    } else {
        return(cc)
    }
}



#' @export
#' @rdname rollcor
rollcor.zoo <- function (x, y, width, show = TRUE, ...) {
    stopifnot(is.zoo(x) & is.zoo(y))
    if (!is.null(dim(x))) {
        stopifnot(ncol(x) == 1)
    }
    if (!is.null(dim(y))) {
        stopifnot(ncol(y) == 1)
    }
    x <- na.trim(x, sides = 'both')
    y <- na.trim(y, sides = 'both')
    if ((length(index(x)) != length(index(y))) || (!all(index(x) == index(y)))) {
        id0 <- max(index(x)[1], index(y)[1])
        id1 <- min(index(x)[length(x)], index(y)[length(y)])
        x <- window(x, start = id0, end = id1)
        y <- window(y, start = id0, end = id1)
        cat('Adjust the time range: ', id0, ' to ', id1, '\n', sep = '')
    }
    stopifnot(all(index(x) == index(y)))


    len <- length(x)
    halfWidth <- (width - 1) / 2
    xcore <- coredata(x)
    ycore <- coredata(y)
    cc <- rollcor(xcore, ycore, width = width, show = FALSE)
    cc <- zoo(cc, order.by = index(x)[halfWidth + 1] : index(x)[len - halfWidth])
    if (show == TRUE) {
        dev.new()
        plot(cc, type = 'o', main = 'Sliding Correlations', xlab = 'Time', ylab = '')
        return(invisible(cc))
    } else {
        return(cc)
    }
}



