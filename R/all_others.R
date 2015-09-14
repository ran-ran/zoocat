
#' @export 
'cummax.zoocat' <- function (x, ...) {
    colAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- 'cummax'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', class(ret))
    return(ret)
}
        

#' @export
'cummax.mlydata' <- function (x, ...) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    ret <- 'cummax'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', class(ret))
    return(ret)
}
        

#' @export 
'cummin.zoocat' <- function (x, ...) {
    colAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- 'cummin'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', class(ret))
    return(ret)
}
        

#' @export
'cummin.mlydata' <- function (x, ...) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    ret <- 'cummin'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', class(ret))
    return(ret)
}
        

#' @export 
'cumprod.zoocat' <- function (x, ...) {
    colAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- 'cumprod'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', class(ret))
    return(ret)
}
        

#' @export
'cumprod.mlydata' <- function (x, ...) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    ret <- 'cumprod'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', class(ret))
    return(ret)
}
        

#' @export 
'cumsum.zoocat' <- function (x, ...) {
    colAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- 'cumsum'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', class(ret))
    return(ret)
}
        

#' @export
'cumsum.mlydata' <- function (x, ...) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    ret <- 'cumsum'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', class(ret))
    return(ret)
}
        

#' @export 
'diff.zoocat' <- function (x, ...) {
    colAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- 'diff'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', class(ret))
    return(ret)
}
        

#' @export
'diff.mlydata' <- function (x, ...) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    ret <- 'diff'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', class(ret))
    return(ret)
}
        

#' @export 
'head.zoocat' <- function (x, ...) {
    colAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- 'head'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', class(ret))
    return(ret)
}
        

#' @export
'head.mlydata' <- function (x, ...) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    ret <- 'head'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', class(ret))
    return(ret)
}
        

#' @export 
'lag.zoocat' <- function (x, ...) {
    colAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- 'lag'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', class(ret))
    return(ret)
}
        

#' @export
'lag.mlydata' <- function (x, ...) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    ret <- 'lag'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', class(ret))
    return(ret)
}
        

#' @export 
'na.aggregate.zoocat' <- function (object, ...) {
    colAttr <- cattr(object)
    object <- as.zoo(object)
    ret <- 'na.aggregate'(object, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', class(ret))
    return(ret)
}
        

#' @export
'na.aggregate.mlydata' <- function (object, ...) {
    month <- attr(object, 'month')
    object <- as.zoo(object)
    ret <- 'na.aggregate'(object, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', class(ret))
    return(ret)
}
        

#' @export 
'na.approx.zoocat' <- function (object, ...) {
    colAttr <- cattr(object)
    object <- as.zoo(object)
    ret <- 'na.approx'(object, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', class(ret))
    return(ret)
}
        

#' @export
'na.approx.mlydata' <- function (object, ...) {
    month <- attr(object, 'month')
    object <- as.zoo(object)
    ret <- 'na.approx'(object, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', class(ret))
    return(ret)
}
        

#' @export 
'na.contiguous.zoocat' <- function (object, ...) {
    colAttr <- cattr(object)
    object <- as.zoo(object)
    ret <- 'na.contiguous'(object, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', class(ret))
    return(ret)
}
        

#' @export
'na.contiguous.mlydata' <- function (object, ...) {
    month <- attr(object, 'month')
    object <- as.zoo(object)
    ret <- 'na.contiguous'(object, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', class(ret))
    return(ret)
}
        

#' @export 
'na.fill.zoocat' <- function (object, ...) {
    colAttr <- cattr(object)
    object <- as.zoo(object)
    ret <- 'na.fill'(object, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', class(ret))
    return(ret)
}
        

#' @export
'na.fill.mlydata' <- function (object, ...) {
    month <- attr(object, 'month')
    object <- as.zoo(object)
    ret <- 'na.fill'(object, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', class(ret))
    return(ret)
}
        

#' @export 
'na.spline.zoocat' <- function (object, ...) {
    colAttr <- cattr(object)
    object <- as.zoo(object)
    ret <- 'na.spline'(object, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', class(ret))
    return(ret)
}
        

#' @export
'na.spline.mlydata' <- function (object, ...) {
    month <- attr(object, 'month')
    object <- as.zoo(object)
    ret <- 'na.spline'(object, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', class(ret))
    return(ret)
}
        

#' @export 
'na.trim.zoocat' <- function (object, ...) {
    colAttr <- cattr(object)
    object <- as.zoo(object)
    ret <- 'na.trim'(object, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', class(ret))
    return(ret)
}
        

#' @export
'na.trim.mlydata' <- function (object, ...) {
    month <- attr(object, 'month')
    object <- as.zoo(object)
    ret <- 'na.trim'(object, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', class(ret))
    return(ret)
}
        

#' @export 
'rollapply.zoocat' <- function (data, ...) {
    colAttr <- cattr(data)
    data <- as.zoo(data)
    ret <- 'rollapply'(data, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', class(ret))
    return(ret)
}
        

#' @export
'rollapply.mlydata' <- function (data, ...) {
    month <- attr(data, 'month')
    data <- as.zoo(data)
    ret <- 'rollapply'(data, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', class(ret))
    return(ret)
}
        

#' @export 
'rollmax.zoocat' <- function (x, ...) {
    colAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- 'rollmax'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', class(ret))
    return(ret)
}
        

#' @export
'rollmax.mlydata' <- function (x, ...) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    ret <- 'rollmax'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', class(ret))
    return(ret)
}
        

#' @export 
'rollmean.zoocat' <- function (x, ...) {
    colAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- 'rollmean'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', class(ret))
    return(ret)
}
        

#' @export
'rollmean.mlydata' <- function (x, ...) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    ret <- 'rollmean'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', class(ret))
    return(ret)
}
        

#' @export 
'rollmedian.zoocat' <- function (x, ...) {
    colAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- 'rollmedian'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', class(ret))
    return(ret)
}
        

#' @export
'rollmedian.mlydata' <- function (x, ...) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    ret <- 'rollmedian'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', class(ret))
    return(ret)
}
        

#' @export 
'rollsum.zoocat' <- function (x, ...) {
    colAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- 'rollsum'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', class(ret))
    return(ret)
}
        

#' @export
'rollsum.mlydata' <- function (x, ...) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    ret <- 'rollsum'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', class(ret))
    return(ret)
}
        

#' @export 
'scale.zoocat' <- function (x, ...) {
    colAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- 'scale'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', class(ret))
    return(ret)
}
        

#' @export
'scale.mlydata' <- function (x, ...) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    ret <- 'scale'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', class(ret))
    return(ret)
}
        

#' @export 
'tail.zoocat' <- function (x, ...) {
    colAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- 'tail'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', class(ret))
    return(ret)
}
        

#' @export
'tail.mlydata' <- function (x, ...) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    ret <- 'tail'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', class(ret))
    return(ret)
}
        

#' @export 
'as.data.frame.zoocat' <- function (x, ...) {
    x <- as.zoo(x)
    ret <- 'as.data.frame'(x, ...)
    return(ret)
}
        

#' @export
'as.data.frame.mlydata' <- function (x, ...) {
    x <- as.zoo(x)
    ret <- 'as.data.frame'(x, ...)
    return(ret)
}
        

#' @export 
'as.matrix.zoocat' <- function (x, ...) {
    x <- as.zoo(x)
    ret <- 'as.matrix'(x, ...)
    return(ret)
}
        

#' @export
'as.matrix.mlydata' <- function (x, ...) {
    x <- as.zoo(x)
    ret <- 'as.matrix'(x, ...)
    return(ret)
}
        

#' @export 
'coredata.zoocat' <- function (x, ...) {
    x <- as.zoo(x)
    ret <- 'coredata'(x, ...)
    return(ret)
}
        

#' @export
'coredata.mlydata' <- function (x, ...) {
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
'plot.mlydata' <- function (x, ...) {
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
'barplot.mlydata' <- function (height, ...) {
    height <- as.zoo(height)
    ret <- 'barplot'(height, ...)
    return(ret)
}
        

#' @export 
'coredata<-.zoocat' <- function (x, value) {
    colAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- 'coredata<-'(x, value)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', class(ret))
    return(ret)
}
        

#' @export
'coredata<-.mlydata' <- function (x, value) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    ret <- 'coredata<-'(x, value)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', class(ret))
    return(ret)
}
        

#' @export 
'index<-.zoocat' <- function (x, value) {
    colAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- 'index<-'(x, value)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', class(ret))
    return(ret)
}
        

#' @export
'index<-.mlydata' <- function (x, value) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    ret <- 'index<-'(x, value)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', class(ret))
    return(ret)
}
        
