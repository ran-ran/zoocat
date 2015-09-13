
#' @rdname window
#' @export
window.mlydataList <- function (x, ...) {
    for (i in 1 : length(x)) {
        x[[i]] <- window(x[[i]], ...)
    }
    return(x)
}


#' @rdname expand
#' @export
expand.mlydataList <- function (x, ...) {
    for (i in 1 : length(x)) {
        x[[i]] <- expand(x[[i]], ...)
    }
    return(x)
}

#' @export
na.trim.mlydataList <- function (x, ...) {
    for (i in 1 : length(x)) {
        x[[i]] <- na.trim(x[[i]], ...)
    }
    return(x)
}


#' @export
normalize.mlydataList <- function (x, ...) {
    for (i in 1 : length(x)) {
        x[[i]] <- normalize(x[[i]], ...)
    }
    return(x)
}




