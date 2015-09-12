
#' @rdname window
#' @export
window.mlydataList <- function (x, ...) {
    for (i in 1 : length(x)) {
        x[[i]] <- window(x[[i]], ...)
    }
    return(x)
}
