
#' @export
'cummax.zoocat' <- function (x, ...) {
    tcolAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- 'cummax'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', 'zoo')
    return(ret)
}
        

#' @export
'cummax.mlydata' <- function (x, ...) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    ret <- 'cummax'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', 'zoo')
    return(ret)
}
        

#' @export
'cummin.zoocat' <- function (x, ...) {
    tcolAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- 'cummin'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', 'zoo')
    return(ret)
}
        

#' @export
'cummin.mlydata' <- function (x, ...) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    ret <- 'cummin'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', 'zoo')
    return(ret)
}
        

#' @export
'cumprod.zoocat' <- function (x, ...) {
    tcolAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- 'cumprod'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', 'zoo')
    return(ret)
}
        

#' @export
'cumprod.mlydata' <- function (x, ...) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    ret <- 'cumprod'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', 'zoo')
    return(ret)
}
        

#' @export
'cumsum.zoocat' <- function (x, ...) {
    tcolAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- 'cumsum'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', 'zoo')
    return(ret)
}
        

#' @export
'cumsum.mlydata' <- function (x, ...) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    ret <- 'cumsum'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', 'zoo')
    return(ret)
}
        

#' @export
'diff.zoocat' <- function (x, ...) {
    tcolAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- 'diff'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', 'zoo')
    return(ret)
}
        

#' @export
'diff.mlydata' <- function (x, ...) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    ret <- 'diff'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', 'zoo')
    return(ret)
}
        

#' @export
'lag.zoocat' <- function (x, ...) {
    tcolAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- 'lag'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', 'zoo')
    return(ret)
}
        

#' @export
'lag.mlydata' <- function (x, ...) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    ret <- 'lag'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', 'zoo')
    return(ret)
}
        

#' @export
'na.aggregate.zoocat' <- function (x, ...) {
    tcolAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- 'na.aggregate'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', 'zoo')
    return(ret)
}
        

#' @export
'na.aggregate.mlydata' <- function (x, ...) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    ret <- 'na.aggregate'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', 'zoo')
    return(ret)
}
        

#' @export
'na.approx.zoocat' <- function (x, ...) {
    tcolAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- 'na.approx'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', 'zoo')
    return(ret)
}
        

#' @export
'na.approx.mlydata' <- function (x, ...) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    ret <- 'na.approx'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', 'zoo')
    return(ret)
}
        

#' @export
'na.contiguous.zoocat' <- function (x, ...) {
    tcolAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- 'na.contiguous'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', 'zoo')
    return(ret)
}
        

#' @export
'na.contiguous.mlydata' <- function (x, ...) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    ret <- 'na.contiguous'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', 'zoo')
    return(ret)
}
        

#' @export
'na.fill.zoocat' <- function (x, ...) {
    tcolAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- 'na.fill'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', 'zoo')
    return(ret)
}
        

#' @export
'na.fill.mlydata' <- function (x, ...) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    ret <- 'na.fill'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', 'zoo')
    return(ret)
}
        

#' @export
'na.spline.zoocat' <- function (x, ...) {
    tcolAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- 'na.spline'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', 'zoo')
    return(ret)
}
        

#' @export
'na.spline.mlydata' <- function (x, ...) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    ret <- 'na.spline'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', 'zoo')
    return(ret)
}
        

#' @export
'na.trim.zoocat' <- function (x, ...) {
    tcolAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- 'na.trim'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', 'zoo')
    return(ret)
}
        

#' @export
'na.trim.mlydata' <- function (x, ...) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    ret <- 'na.trim'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', 'zoo')
    return(ret)
}
        

#' @export
'rollapply.zoocat' <- function (x, ...) {
    tcolAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- 'rollapply'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', 'zoo')
    return(ret)
}
        

#' @export
'rollapply.mlydata' <- function (x, ...) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    ret <- 'rollapply'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', 'zoo')
    return(ret)
}
        

#' @export
'rollmax.zoocat' <- function (x, ...) {
    tcolAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- 'rollmax'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', 'zoo')
    return(ret)
}
        

#' @export
'rollmax.mlydata' <- function (x, ...) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    ret <- 'rollmax'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', 'zoo')
    return(ret)
}
        

#' @export
'rollmean.zoocat' <- function (x, ...) {
    tcolAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- 'rollmean'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', 'zoo')
    return(ret)
}
        

#' @export
'rollmean.mlydata' <- function (x, ...) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    ret <- 'rollmean'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', 'zoo')
    return(ret)
}
        

#' @export
'rollmedian.zoocat' <- function (x, ...) {
    tcolAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- 'rollmedian'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', 'zoo')
    return(ret)
}
        

#' @export
'rollmedian.mlydata' <- function (x, ...) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    ret <- 'rollmedian'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', 'zoo')
    return(ret)
}
        

#' @export
'rollsum.zoocat' <- function (x, ...) {
    tcolAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- 'rollsum'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', 'zoo')
    return(ret)
}
        

#' @export
'rollsum.mlydata' <- function (x, ...) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    ret <- 'rollsum'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', 'zoo')
    return(ret)
}
        

#' @export
'coredata<-.zoocat' <- function (x, ...) {
    tcolAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- 'coredata<-'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', 'zoo')
    return(ret)
}
        

#' @export
'coredata<-.mlydata' <- function (x, ...) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    ret <- 'coredata<-'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', 'zoo')
    return(ret)
}
        
