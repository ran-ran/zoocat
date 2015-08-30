
#' Coercing Objects to Class "zoocat"
#' 
#' Coercing objects to class "zoocat".
#' 
#' @param x An object.
#' @return A zoocat object.
#' @examples
#' 
#' x <- matrix(1 : 20, nrow = 5)
#' md <- mlydata(x, year = 1991 : 1995, month = c(2, 3, 5, 6))
#' zc <- as.zoocat(md)
#' 
#' x <- matrix(1 : 20, nrow = 5)
#' md1 <- mlydata(x, year = 1991 : 1995, month = c(2, 3, 5, 6))
#' md2 <- md1 + 1
#' mdList <- mlydataList(list(md1 = md1, md2 = md2))
#' zc <- as.zoocat(mdList)
#' 
#' zobj <- zoo(matrix(1:10, nrow = 5), order.by = 11:15)
#' colnames(zobj) <- c('a', 'b')
#' as.zoocat(zobj)
#' 
#' @name as.zoocat
#' @rdname as.zoocat
#' @export
as.zoocat <- function (x, ...) { UseMethod('as.zoocat') }

#'
#' @export as.zoocat.mlydata
#' @rdname as.zoocat
#' @param varname The value for the name field in the \code{cattr} of 
#' the output \code{zoocat} object.
as.zoocat.mlydata <- function (x, varname = NULL) {
    if (is.null(varname)) {
        sysN <- sys.nframe()
        cl <- sys.call(sysN - 1)
        varname <- as.character(cl[2])
    }
    year <- index(x)
    cAttr <- data.frame(name = varname, month = attr(x, 'month'), stringsAsFactors = FALSE)
    z <- zoocat(coredata(x), order.by = year, colattr = cAttr,
                frequency = 1)
    return(z)
}


#'
#' @export as.zoocat.mlydataList
#' @rdname as.zoocat
as.zoocat.mlydataList <- function (x) {
    varname <- names(x)
    for (i in 1 : length(x)) {
        x[[i]] <- as.zoocat(x[[i]], varname[i])
    }
    x <- unclass(x)
    zc <- do.call(merge, args = x)
    return(zc)
}


#' @export as.zoocat.zoo
#' @rdname as.zoocat
as.zoocat.zoo <- function (x, colattr = NULL) {
    stopifnot(length(dim(x))== 2)
    if (is.null(colattr)) {
        stopifnot(!is.null(colnames(x)))
        colattr <- data.frame(name = colnames(x))
    }
    attr(x, 'cattr') <- colattr
    class(x) <- c('zoocat', class(x))
    return(x)
}


