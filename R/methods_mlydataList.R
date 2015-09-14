
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
na.trim.mlydataList <- function (object, ...) {
    for (i in 1 : length(object)) {
        object[[i]] <- na.trim(object[[i]], ...)
    }
    return(object)
}


#' @export
normalize.mlydataList <- function (x, ...) {
    for (i in 1 : length(x)) {
        x[[i]] <- normalize(x[[i]], ...)
    }
    return(x)
}




