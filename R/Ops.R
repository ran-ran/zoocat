


#' @export
Ops.zoocat <- function (e1, e2) {
    colAttr <- attr(e1, 'cattr')
    e1 <- as.zoo(e1)
    if (!missing(e2) & inherits(e2, 'zoocat')) {
        if (!identical(colAttr, attr(e2, 'cattr'))) {
            stop('cattr of objects must be identical.')
        }
        e2 <- as.zoo(e2)
    }
    if (missing(e2)) {
        e <- NextMethod(.Generic)
    } else if (any(nchar(.Method) == 0L)) {
        e <- NextMethod(.Generic)
    } else {
        merge(e1, e2, all = FALSE, retclass = NULL)
        e <- NextMethod(.Generic)
    }
    if (is.null(attr(e, "index"))) {
        if (!missing(e2) && nchar(.Method)[1L] == 0L) {
            out <- zoo(e, index(e2), attr(e2, "frequency"))
        }
        else {
            out <- zoo(e, index(e1), attr(e1, "frequency"))
        }
    } else {
        out <- e
    }
    attributes(out)$dimnames <- NULL
    cattr(out) <- colAttr
    return(out)
}



