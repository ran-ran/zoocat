
#' Coercion Objects to Class \code{zoo}
#' 
#' Coercing objects to class {zoo}.
#' 
#' For methods as.zoo.mlydata and as.zoo.zoocat, the returned zoo object will
#' be added column names automatically.
#' 
#' @param x An object.
#' @return A zoo object.
#' @examples
#' 
#' x <- matrix(1 : 20, nrow = 5)
#' md <- mlydata(x, year = 1991 : 1995, month = c(2, 3, 5, 6))
#' z <- as.zoo(md)
#' 
#' x <- matrix(1 : 20, nrow = 5)
#' md1 <- mlydata(x, year = 1991 : 1995, month = c(2, 3, 5, 6))
#' md2 <- md1 + 1
#' mdList <- mlydataList(list(md1 = md1, md2 = md2))
#' zc <- as.zoocat(mdList)
#' as.zoo(zc)
#' 
#' @rdname as.zoo
#' @name as.zoo
#' @export
as.zoo.mlydata <- function(x) {
    class(x) <- 'zoo'
    colnames(x) <- month2Str(attr(x, 'month'))
    attr(x, 'month') <- NULL
    return(x)
}

#' @rdname as.zoo
#' @export
as.zoo.zoocat <- function (x) {
    if (length(x) == 0){
        return(zoo())
    } else {
        colnames(x) <- cattr2name(attr(x, 'cattr'))
        attr(x, 'cattr') <- NULL
        class(x) <- 'zoo'
        return(x)
    }
}


