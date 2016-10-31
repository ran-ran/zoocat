#' Determine the validity of a "\code{zoocat}" Object
#' 
#' Determine the validity of a "\code{zoocat}" object.
#' 
#' @usage isvalid(x)
#' @param x A \code{zoocat} object.
#' @return A logic variable.
#' @examples
#' 
#' x <- matrix(1 : 20, nrow = 5)
#' colAttr <- data.frame(month = c(2, 3, 5, 6), name = 'sst')
#' zc <- zoocat(x, order.by = 1991 : 1995, colattr = colAttr)
#' isvalid(zc)
#' 
#' @export
#' @name isvalid
#' @rdname isvalid
isvalid <- function (x) {
    UseMethod('isvalid')
}


#' @export
#' @rdname isvalid
isvalid.zoocat <- function (x) {
    if (length(x) == 0) {
        cat('empty zoocat\n')
        return(1)
    }
    isV <- 1
    cAttr <- attr(x, 'cattr')
    if (!is.matrix(cAttr) & !is.data.frame(cAttr)) {
        cat('The cattr must be matrix or dataframe.\n')
        isV <- 0
    } else {
        if (nrow(cAttr) != ncol(x)) {
            cat('The dimension of cattr is incorrect.\n')
            isV <- 0
        }
        if (is.null(colnames(cAttr))) {
            cat('The cattr has no colnames.\n')
            isV <- 0
        }
    }
    return(isV)
}
