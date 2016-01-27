
#' @export 
'cummax.zoocat' <- function (x, ...) {
    class0 <- class(x)
    colAttr <- cattr(x)
    indexName <- indname(x)
    x <- as.zoo(x, add.colname = FALSE)
    ret <- 'cummax'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    attr(ret, 'indname') <- indexName
    class(ret) <- class0
    return(ret)
}
        

#' @export 
'cummin.zoocat' <- function (x, ...) {
    class0 <- class(x)
    colAttr <- cattr(x)
    indexName <- indname(x)
    x <- as.zoo(x, add.colname = FALSE)
    ret <- 'cummin'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    attr(ret, 'indname') <- indexName
    class(ret) <- class0
    return(ret)
}
        

#' @export 
'cumprod.zoocat' <- function (x, ...) {
    class0 <- class(x)
    colAttr <- cattr(x)
    indexName <- indname(x)
    x <- as.zoo(x, add.colname = FALSE)
    ret <- 'cumprod'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    attr(ret, 'indname') <- indexName
    class(ret) <- class0
    return(ret)
}
        

#' @export 
'cumsum.zoocat' <- function (x, ...) {
    class0 <- class(x)
    colAttr <- cattr(x)
    indexName <- indname(x)
    x <- as.zoo(x, add.colname = FALSE)
    ret <- 'cumsum'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    attr(ret, 'indname') <- indexName
    class(ret) <- class0
    return(ret)
}
        

#' @export 
'diff.zoocat' <- function (x, ...) {
    class0 <- class(x)
    colAttr <- cattr(x)
    indexName <- indname(x)
    x <- as.zoo(x, add.colname = FALSE)
    ret <- 'diff'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    attr(ret, 'indname') <- indexName
    class(ret) <- class0
    return(ret)
}
        

#' @export 
'head.zoocat' <- function (x, ...) {
    class0 <- class(x)
    colAttr <- cattr(x)
    indexName <- indname(x)
    x <- as.zoo(x, add.colname = FALSE)
    ret <- 'head'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    attr(ret, 'indname') <- indexName
    class(ret) <- class0
    return(ret)
}
        

#' @export 
'lag.zoocat' <- function (x, ...) {
    class0 <- class(x)
    colAttr <- cattr(x)
    indexName <- indname(x)
    x <- as.zoo(x, add.colname = FALSE)
    ret <- 'lag'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    attr(ret, 'indname') <- indexName
    class(ret) <- class0
    return(ret)
}
        

#' @export 
'na.aggregate.zoocat' <- function (object, ...) {
    class0 <- class(object)
    colAttr <- cattr(object)
    indexName <- indname(object)
    object <- as.zoo(object, add.colname = FALSE)
    ret <- 'na.aggregate'(object, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    attr(ret, 'indname') <- indexName
    class(ret) <- class0
    return(ret)
}
        

#' @export 
'na.approx.zoocat' <- function (object, ...) {
    class0 <- class(object)
    colAttr <- cattr(object)
    indexName <- indname(object)
    object <- as.zoo(object, add.colname = FALSE)
    ret <- 'na.approx'(object, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    attr(ret, 'indname') <- indexName
    class(ret) <- class0
    return(ret)
}
        

#' @export 
'na.contiguous.zoocat' <- function (object, ...) {
    class0 <- class(object)
    colAttr <- cattr(object)
    indexName <- indname(object)
    object <- as.zoo(object, add.colname = FALSE)
    ret <- 'na.contiguous'(object, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    attr(ret, 'indname') <- indexName
    class(ret) <- class0
    return(ret)
}
        

#' @export 
'na.fill.zoocat' <- function (object, ...) {
    class0 <- class(object)
    colAttr <- cattr(object)
    indexName <- indname(object)
    object <- as.zoo(object, add.colname = FALSE)
    ret <- 'na.fill'(object, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    attr(ret, 'indname') <- indexName
    class(ret) <- class0
    return(ret)
}
        

#' @export 
'na.spline.zoocat' <- function (object, ...) {
    class0 <- class(object)
    colAttr <- cattr(object)
    indexName <- indname(object)
    object <- as.zoo(object, add.colname = FALSE)
    ret <- 'na.spline'(object, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    attr(ret, 'indname') <- indexName
    class(ret) <- class0
    return(ret)
}
        

#' @export 
'na.trim.zoocat' <- function (object, ...) {
    class0 <- class(object)
    colAttr <- cattr(object)
    indexName <- indname(object)
    object <- as.zoo(object, add.colname = FALSE)
    ret <- 'na.trim'(object, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    attr(ret, 'indname') <- indexName
    class(ret) <- class0
    return(ret)
}
        

#' @export 
'rollapply.zoocat' <- function (data, ...) {
    class0 <- class(data)
    colAttr <- cattr(data)
    indexName <- indname(data)
    data <- as.zoo(data, add.colname = FALSE)
    ret <- 'rollapply'(data, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    attr(ret, 'indname') <- indexName
    class(ret) <- class0
    return(ret)
}
        

#' @export 
'rollmax.zoocat' <- function (x, ...) {
    class0 <- class(x)
    colAttr <- cattr(x)
    indexName <- indname(x)
    x <- as.zoo(x, add.colname = FALSE)
    ret <- 'rollmax'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    attr(ret, 'indname') <- indexName
    class(ret) <- class0
    return(ret)
}
        

#' @export 
'rollmean.zoocat' <- function (x, ...) {
    class0 <- class(x)
    colAttr <- cattr(x)
    indexName <- indname(x)
    x <- as.zoo(x, add.colname = FALSE)
    ret <- 'rollmean'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    attr(ret, 'indname') <- indexName
    class(ret) <- class0
    return(ret)
}
        

#' @export 
'rollmedian.zoocat' <- function (x, ...) {
    class0 <- class(x)
    colAttr <- cattr(x)
    indexName <- indname(x)
    x <- as.zoo(x, add.colname = FALSE)
    ret <- 'rollmedian'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    attr(ret, 'indname') <- indexName
    class(ret) <- class0
    return(ret)
}
        

#' @export 
'rollsum.zoocat' <- function (x, ...) {
    class0 <- class(x)
    colAttr <- cattr(x)
    indexName <- indname(x)
    x <- as.zoo(x, add.colname = FALSE)
    ret <- 'rollsum'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    attr(ret, 'indname') <- indexName
    class(ret) <- class0
    return(ret)
}
        

#' @export 
'scale.zoocat' <- function (x, ...) {
    class0 <- class(x)
    colAttr <- cattr(x)
    indexName <- indname(x)
    x <- as.zoo(x, add.colname = FALSE)
    ret <- 'scale'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    attr(ret, 'indname') <- indexName
    class(ret) <- class0
    return(ret)
}
        

#' @export 
'tail.zoocat' <- function (x, ...) {
    class0 <- class(x)
    colAttr <- cattr(x)
    indexName <- indname(x)
    x <- as.zoo(x, add.colname = FALSE)
    ret <- 'tail'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    attr(ret, 'indname') <- indexName
    class(ret) <- class0
    return(ret)
}
        

#' @export 
'window.zoocat' <- function (x, ...) {
    class0 <- class(x)
    colAttr <- cattr(x)
    indexName <- indname(x)
    x <- as.zoo(x, add.colname = FALSE)
    ret <- 'window'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    attr(ret, 'indname') <- indexName
    class(ret) <- class0
    return(ret)
}
        

#' @export 
'as.matrix.zoocat' <- function (x, ...) {
    x <- as.zoo(x)
    ret <- 'as.matrix'(x, ...)
    return(ret)
}
        

#' @export 
'as.data.frame.zoocat' <- function (x, ...) {
    x <- as.zoo(x)
    ret <- 'as.data.frame'(x, ...)
    return(ret)
}
        

#' @export 
'coredata.zoocat' <- function (x, ...) {
    x <- as.zoo(x)
    ret <- 'coredata'(x, ...)
    return(ret)
}
        

#' @export 
'plot.zoocat' <- function (x, ...) {
    x <- as.zoo(x)
    ret <- 'plot'(x, ...)
    return(ret)
}
        

#' @export 
'barplot.zoocat' <- function (height, ...) {
    height <- as.zoo(height)
    ret <- 'barplot'(height, ...)
    return(ret)
}
        

#' @export 
'index<-.zoocat' <- function (x, value) {
    colAttr <- cattr(x)
    x <- as.zoo(x, add.colname = FALSE)
    ret <- 'index<-'(x, value)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', class(ret))
    return(ret)
}
        
