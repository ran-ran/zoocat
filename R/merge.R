
#' Combine \code{zoocat} or \code{mlydata} Objects by Columns
#' 
#' Combine \code{zoocat} or \code{mlydata} objects by columns.
#' 
#' \code{merge.zoocat} and \code{merge.mlydata} are the extensions of \code{merge.zoo}.
#' For \code{merge.zoocat}, when combining \code{cattr}, some NA will be filled in if it is
#' necessary. \cr
#' The arguments \code{all} and \code{fill} are used same as in \code{merge.zoo}.
#' The arguments \code{suffixes}, \code{check.names}, \code{retclass} 
#' and \code{drop} are not used.
#'
#' @usage
#' ## S3 method for class "zoocat"
#' merge(...)
#' @param ...  \code{zoocat} or \code{mlydata} objects.
#' @param all,fill,suffixes,check.names,retclass,drop See details.
#' @return \code{merge.zoocat} will return a \code{zoocat} object.
#'  \code{merge.mlydata} will return a \code{mlydata} object.
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
#' x <- matrix(1 : 20, nrow = 5)
#' md1 <- mlydata(x, year = 1991 : 1995, month = c(2, 3, 5, 6))
#' md2 <- adjust_ym(md1, k = 1)
#' merge(md2, md1)
#' cbind(md2, md1)
#'
#' @export
#' @rdname merge
#' @name merge
merge.zoocat <- function (..., all = TRUE, fill = NA, suffixes = NULL,
                          check.names = FALSE, retclass = 'zoocat',
                          drop = TRUE) {
    listin <- list(...)
    numZoo <- length(listin)
    cattrList <- list()
    for (i in 1 : numZoo) {
        stopifnot(class(listin[[i]])[1] == 'zoocat')
        cattrList[[i]] <- attr(listin[[i]], 'cattr')
        listin[[i]] <- as.zoo(listin[[i]])
    }
    cattrTotal <- plyr::rbind.fill(cattrList)
    zooTotal <- do.call(merge, 
                      args = c(listin, list(all = all, fill = fill,
                                            suffixes = NULL,
                                            check.names = FALSE,
                                            retclass = 'zoo',
                                            drop = TRUE))) 
    colnames(zooTotal) <- NULL
    attr(zooTotal, 'cattr') <- cattrTotal
    class(zooTotal) <- c('zoocat', class(listin[[1]]))
    return(zooTotal)
}


#' @export
#' @rdname merge
#' @usage 
#' ## S3 method for class "mlydata"
#' merge(...)
merge.mlydata <- function (..., all = TRUE, fill = NA, suffixes = NULL,
                           check.names = FALSE, retclass = 'mlydata',
                           drop = TRUE) {
    listin <- list(...)
    month <- c()
    for (i in 1 : length(listin)) {
        stopifnot(class(listin[[i]])[1] == 'mlydata')
        month <- c(month, attr(listin[[i]], 'month'))
        listin[[i]] <- as.zoo(listin[[i]])
    }
    zooobj <- do.call(merge, 
                      args = c(listin, list(all = all, fill = fill,
                                            suffixes = NULL,
                                            check.names = FALSE,
                                            retclass = 'zoo',
                                            drop = TRUE))) 
    colnames(zooobj) <- NULL
    if (!is.null(retclass)) {
        ret <- as.mlydata(zooobj, month = month)
        return(ret)
    }
}



#' @export
#' @rdname merge
cbind.zoocat <- function (...) {
    return(merge(...))
}


#' @export
#' @rdname merge
cbind.mlydata <- function (...) {
    return(merge(...))
}
