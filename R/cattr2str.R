

cattr2str <- function (colattr) {
    return(do.call(function(...) paste(..., sep = '_'), colattr))
}

