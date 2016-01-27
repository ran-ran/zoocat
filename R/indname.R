

#' Get or set the name of the index variable
#' 
#' @examples
#' x <- matrix(1 : 20, nrow = 5)
#' colAttr <- data.frame(month = c(2, 3, 5, 6), name = c(rep('xxx', 3), 'yyy'))
#' zc <- zoocat(x, order.by = 1991 : 1995, colattr = colAttr)
#' indname(zc)
#' indname(zc) <- 'time'
#' 
#' @name indname
#' @rdname indname
#' @export
#' @param x the object.
#' @param value the new value.
indname <- function (x) {
    UseMethod('indname')
}

#' @name indname
#' @rdname indname
#' @export
'indname<-' <- function (x, value) {
    UseMethod('indname<-')
}


#' @name indname
#' @rdname indname
#' @export
indname.zoocat <- function (x) {
    return(attr(x, 'indname'))
}


#' @name indname
#' @rdname indname
#' @export
'indname<-.zoocat' <- function (x, value) {
    attr(x, 'indname') <- value
    return(x)
}


