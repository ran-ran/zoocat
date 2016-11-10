
#' Principal components analysis of a \code{zoocat} object
#' 
#' Principal components analysis of a "\code{zoocat}" object. This function is a 
#' wrap of the \code{stats::prcomp}. The \code{rotation} returned by \code{stats::prcomp}
#' is binded with the \code{cattr} table to be a data frame, and the \code{x} returned 
#' by \code{stats::prcomp} is binded with the \code{index} to be a "\code{zoo}" object.
#' 
#' @return a list with following elements: \code{prcomp.obj} (the object returned by \code{stats::prcomp}),
#' \code{rotation} (the data frame containing information of the variable loadings),
#' \code{z} (the \code{zoo} object containing the rotated data).
#' 
#' @rdname pca
#' @export
#' @param x the zoocat object.
#' @param ... other argument.
#' @examples 
#' x <- matrix(1 : 20, nrow = 5)
#' colAttr <- data.frame(month = c(2, 3, 5, 6), name = c(rep('xxx', 3), 'yyy'))
#' zc <- zoocat(x, order.by = 1991 : 1995, colattr = colAttr)
#' pca <- prcomp(zc)
#' print(pca$rotation)
#' print(pca$z)
#' 
prcomp.zoocat <- function (x, ...) {
    prcomp.obj <- prcomp(coredata(x), ...)
    rotation <- data.frame(cattr(x), prcomp.obj$rotation)
    rownames(rotation) <- NULL
    if (!is.null(prcomp.obj$x)) {
        z <- zoo(prcomp.obj$x, order.by = index(x))
        ret <- list(prcomp.obj = prcomp.obj,  rotation = rotation, z = z)
    } else {
        ret <- list(prcomp.obj = prcomp.obj,  rotation = rotation)
    }
    return(ret)
}



