

#' Get or set the name of the index variable
#' 
#' @examples
#' x <- matrix(1 : 20, nrow = 5)
#' colAttr <- data.frame(month = c(2, 3, 5, 6), name = c(rep('xxx', 3), 'yyy'))
#' zc <- zoocat(x, order.by = 1991 : 1995, colattr = colAttr)
#' index.name(zc)
#' index.name(zc) <- 'time'
#' 
#' @name index.name
#' @rdname index.name
#' @export
#' @param x the object.
#' @param value the new value.
index.name <- function (x) {
    UseMethod('index.name')
}

#' @name index.name
#' @rdname index.name
#' @export
'index.name<-' <- function (x, value) {
    UseMethod('index.name<-')
}


#' @name index.name
#' @rdname index.name
#' @export
index.name.zoocat <- function (x) {
    return(attr(x, 'index.name'))
}


#' @name index.name
#' @rdname index.name
#' @export
'index.name<-.zoocat' <- function (x, value) {
    attr(x, 'index.name') <- value
    return(x)
}


