
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
#' x <- matrix(1 : 20, nrow = 5)
#' md1 <- mlydata(x, year = 1991 : 1995, month = c(2, 3, 5, 6))
#' md2 <- md1 + 1
#' mdList <- mlydataList(list(md1 = md1, md2 = md2))
#' zc <- as.zoocat(mdList)
#' 
#' @name as.zoocat
#' @rdname as.zoocat
#' @export
as.zoocat <- function (x, ...) { UseMethod('as.zoocat') }

#'
#' @export
#' @rdname as.zoocat
as.zoocat.mlydata <- function (x, varname = NULL) {
    if (is.null(varname)) {
        sysN <- sys.nframe()
        cl <- sys.call(sysN - 1)
        varname <- as.character(cl[2])
    }
    year <- index(x)
    cAttr <- data.frame(name = varname, month = attr(x, 'month'), stringsAsFactors = FALSE)
    z <- zoocat(coredata(x), order.by = year, colattr = cAttr)
    return(z)
}


#'
#' @export
#' @rdname as.zoocat
as.zoocat.mlydataList <- function (x) {
    varname <- names(x)
    zc <- zoocat()
    for (i in 1 : length(x)) {
        zcNow <- as.zoocat(x[[i]], varname[i])
        zc <- cbind(zc, zcNow)
    }
    return(zc)
}




