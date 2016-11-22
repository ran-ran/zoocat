
#' @export
'[.zoocat' <- function(x, i, j, drop = TRUE, ...) {
    if (length(x) == 0) {
        return(zoocat())
    }
    if (missing(i)) {
        i <- 1 : nrow(x)
    }
    if (missing(j)) {
        j <- 1 : ncol(x)
    }
    class0 <- class(x)
    colAttr <- attr(x, 'cattr')
    indexName <- attr(x, 'indname')
    
    if (is.character(j)) {
        cattrStr <- cattr2str(colAttr)
        j <- sapply(j, FUN = function (j1) which(j1 == cattrStr)[1])
        if (any(is.na(j))) {
            stop('Some column does not exist.')
        }
    }
    colAttr <- colAttr[j, , drop = FALSE]
    class(x) <- class(x)[class(x) %in% c('zooreg', 'zoo')]
    x <- x[i, j, drop = FALSE]
    
    if (drop == TRUE & min(dim(x)) == 1) {
        if (nrow(x) == 1 & ncol(x) > 1) {
            x <- as.vector(x)
            names(x) <- cattr2str(colAttr)
        } else if (nrow(x) > 1 & ncol(x) == 1) {
            x <- zoo(as.vector(x), order.by = index(x))
        } else {
            x <- as.vector(x)
        }
    } else { 
        attr(x, 'cattr') <- colAttr
        attr(x, 'indname') <- indexName
        class(x) <- class0
    }
    
    return(x)
}




#' @export
'[<-.zoocat' <- function(x, i, j, value) {
    if (missing(i)) {
        i <- 1 : nrow(x)
    }
    if (missing(j)) {
        j <- 1 : ncol(x)
    }
    class0 <- class(x)
    colnames(x) <- cattr2str(cattr(x))
    class(x) <- class(x)[class(x) %in% c('zooreg', 'zoo')]
    x[i, j] <- value
    colnames(x) <- NULL
    class(x) <- class0
    return(x)
}

