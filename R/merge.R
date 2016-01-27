
#' Combine \code{zoocat} or \code{zoomly} Objects by Columns
#' 
#' Combine \code{zoocat} or \code{zoomly} objects by columns.
#' 
#' \code{merge.zoocat} and \code{merge.zoomly} are the extensions of \code{merge.zoo}.
#' For \code{merge.zoocat}, when combining \code{cattr}, some NA will be filled in if it is
#' necessary. \cr
#' The arguments \code{all} and \code{fill} are used same as in \code{merge.zoo}.
#' The arguments \code{suffixes}, \code{check.names}, \code{retclass} 
#' and \code{drop} are not used.
#'
#' @param ...  \code{zoocat} or \code{zoomly} objects.
#' @param all,fill,suffixes,check.names,retclass,drop See details.
#' @return \code{merge.zoocat} will return a \code{zoocat} object.
#'  \code{merge.zoomly} will return a \code{zoomly} object.
#' @examples
#' 
#' x1 <- matrix(1 : 20, nrow = 5)
#' colAttr <- data.frame(month = c(2, 3, 5, 6), name = 'xxx')
#' zc1 <- zoocat(x1, order.by = 1991 : 1995, colattr = colAttr)
#' x2 <- x1 + 100
#' colAttr2 <- data.frame(modified = TRUE, month = c(4, 6, 7, 9))
#' zc2 <- zoocat(x2, order.by = 1991 : 1995, colattr = colAttr2)
#' merge(zc1, zc2)
#' merge(zc1, lag(zc2), fill = -999)
#' merge(zc1, lag(zc2), all = FALSE)
#' cbind(zc1, zc2)
#'
#' @export
#' @rdname merge
#' @name merge
merge.zoocat <- function (..., all = TRUE, fill = NA, suffixes = NULL,
                          check.names = FALSE, retclass = 'zoocat',
                          drop = TRUE) {
    listin <- list(...)
    class0 <- class(listin[[1]])
    indexName <- attr(listin[[1]], 'indname')
    numZoo <- length(listin)
    cattrList <- list()
    for (i in 1 : numZoo) {
        stopifnot(inherits(listin[[i]], 'zoocat'))
        cattrList[[i]] <- attr(listin[[i]], 'cattr')
        listin[[i]] <- as.zoo(listin[[i]], add.colname = FALSE)
    }
    cattrTotal <- plyr::rbind.fill(cattrList)
    zooTotal <- do.call(merge.zoo, 
                      args = c(listin, list(all = all, fill = fill,
                                            suffixes = NULL,
                                            check.names = FALSE,
                                            retclass = 'zoo',
                                            drop = TRUE))) 
    colnames(zooTotal) <- NULL
    attr(zooTotal, 'cattr') <- cattrTotal
    attr(zooTotal, 'indname') <- indexName
    class(zooTotal) <- class0
    return(zooTotal)
}



#' @export
#' @rdname merge
cbind.zoocat <- function (...) {
    return(merge(...))
}


