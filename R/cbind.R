
#' Combine \code{zoocat} or \code{mlydata} Objects by Columns
#' 
#' Combine \code{zoocat} or \code{mlydata} objects by columns.
#' 
#' cbind.zoocat is similar with cbind.zoo, and the column attributes are also
#' combined. For the combination of cattr, some NA will be filled in if it is
#' necessary. cbind.mlydata will return a \code{zoo} object.
#'
#' @usage
#' ## S3 method for class "zoocat"
#' cbind(...)
#' @param ...  \code{zoocat} or \code{mlydata} objects.
#' @return \code{cbind.zoocat} will return a \code{zoocat} object. \code{cbind.mlydata} will return a \code{zoo} object.
#' @examples
#' 
#' x <- matrix(1 : 20, nrow = 5)
#' colAttr <- data.frame(month = c(2, 3, 5, 6), name = 'xxx')
#' zc <- zoocat(x, order.by = 1991 : 1995, colattr = colAttr)
#' x2 <- x + 100
#' colAttr2 <- data.frame(modified = TRUE, month = c(4, 6, 7, 9))
#' zc2 <- zoocat(x2, order.by = 1991 : 1995, colattr = colAttr2)
#' zc3 <- cbind(zc, zc2)
#' cattr(zc3)
#' 
#' x <- matrix(1 : 20, nrow = 5)
#' md1 <- mlydata(x, year = 1991 : 1995, month = c(2, 3, 5, 6))
#' md2 <- md1 + 1
#' cbind(md1, md2)
#'
#' @export
#' @rdname cbind
#' @name cbind
cbind.zoocat <- function (...) {
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
        zooTotal <- cbind(zooTotal, zooNow)
    }
    attr(zooTotal, 'cattr') <- cattrTotal
    class(zooTotal) <- c('zoocat', 'zoo')
    colnames(zooTotal) <- NULL
    return(zooTotal)
}


#' @export
#' @rdname cbind
#' @usage 
#' ## S3 method for class "mlydata"
#' cbind(...)
cbind.mlydata <- function (...) {
    listin <- list(...)
    stopifnot(length(listin) >= 2)
    ret <- as.zoo(listin[[1]])
    for (i in 2 : length(listin)) {
        objNew <- as.zoo(listin[[i]])
        namevec <- c(colnames(ret), colnames(objNew))
        ret <- cbind(ret, objNew)
        colnames(ret) <- namevec
    }
    return(ret)
}
