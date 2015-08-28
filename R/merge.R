
#' Combine \code{zoocat} or \code{mlydata} Objects by Columns
#' 
#' Combine \code{zoocat} or \code{mlydata} objects by columns.
#' 
#' merge.zoocat and merge.mlydata are similar with merge.zoo.
#' For merge.zoocat, when combining cattr, some NA will be filled in if it is
#' necessary. 
#'
#' @usage
#' ## S3 method for class "zoocat"
#' merge(...)
#' @param ...  \code{zoocat} or \code{mlydata} objects.
#' @return \code{merge.zoocat} will return a \code{zoocat} object.
#'  \code{merge.mlydata} will return a \code{mlydata} object.
#' @examples
#' 
#' x <- matrix(1 : 20, nrow = 5)
#' colAttr <- data.frame(month = c(2, 3, 5, 6), name = 'xxx')
#' zc <- zoocat(x, order.by = 1991 : 1995, colattr = colAttr)
#' x2 <- x + 100
#' colAttr2 <- data.frame(modified = TRUE, month = c(4, 6, 7, 9))
#' zc2 <- zoocat(x2, order.by = 1991 : 1995, colattr = colAttr2)
#' zc3 <- merge(zc, zc2)
#' zc4 <- cbind(zc, zc2)
#' cattr(zc3)
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
merge.zoocat <- function (...) {
    listin <- list(...)
    numZoo <- length(listin)
    idEmpty <- c()
    for (i in 1 : numZoo) {
        if (length(listin[[i]]) == 0) {
            idEmpty <- c(idEmpty, i)
        }
    }
    if (length(idEmpty) > 0) {
        listin <- listin[-idEmpty]
    }
    if (length(listin) == 0) {
        return(zoocat())
    }
    numZoo <- numZoo - length(idEmpty)
    cattrList <- list()
    for (i in 1 : numZoo) {
        stopifnot(class(listin[[i]])[1] == 'zoocat')
        cattrList[[i]] <- attr(listin[[i]], 'cattr')
    }
    cattrTotal <- plyr::rbind.fill(cattrList)
    zooTotal <- zoo()
    for (i in 1 : numZoo) {
        zooNow <- listin[[i]]
        class(zooNow) <- 'zoo'
        zooTotal <- merge(zooTotal, zooNow)
    }
    attr(zooTotal, 'cattr') <- cattrTotal
    class(zooTotal) <- c('zoocat', 'zoo')
    colnames(zooTotal) <- NULL
    return(zooTotal)
}


#' @export
#' @rdname merge
#' @usage 
#' ## S3 method for class "mlydata"
#' merge(...)
merge.mlydata <- function (...) {
    listin <- list(...)
    for (i in 1 : length(listin)) {
        stopifnot(class(listin[[i]])[1] == 'mlydata')
    }
    stopifnot(length(listin) >= 2)
    zooobj <- as.zoo(listin[[1]])
    month <- attr(listin[[1]], 'month')
    for (i in 2 : length(listin)) {
        objNew <- as.zoo(listin[[i]])
        zooobj <- merge(zooobj, objNew)
        month <- c(month, attr(listin[[i]], 'month'))
    }
    ret <- as.mlydata(zooobj, month = month)
    return(ret)
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
