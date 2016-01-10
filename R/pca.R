


#' Principal components analysis of a zoocat object
#' 
#' 
#' @rdname pca
#' @name pca
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
    }
    ret <- list(prcomp.obj = prcomp.obj, 
                rotation = rotation,
                z = z)
    return(ret)
}



