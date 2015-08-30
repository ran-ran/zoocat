
#' @export cummax.zoocat
'cummax.zoocat' <- function (x, ...) {
    tcolAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- 'cummax'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', class(ret))
    return(ret)
}
        

#' @export cummax.mlydata
'cummax.mlydata' <- function (x, ...) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    ret <- 'cummax'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', class(ret))
    return(ret)
}
        

#' @export cummin.zoocat
'cummin.zoocat' <- function (x, ...) {
    tcolAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- 'cummin'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', class(ret))
    return(ret)
}
        

#' @export cummin.mlydata
'cummin.mlydata' <- function (x, ...) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    ret <- 'cummin'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', class(ret))
    return(ret)
}
        

#' @export cumprod.zoocat
'cumprod.zoocat' <- function (x, ...) {
    tcolAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- 'cumprod'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', class(ret))
    return(ret)
}
        

#' @export cumprod.mlydata
'cumprod.mlydata' <- function (x, ...) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    ret <- 'cumprod'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', class(ret))
    return(ret)
}
        

#' @export cumsum.zoocat
'cumsum.zoocat' <- function (x, ...) {
    tcolAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- 'cumsum'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', class(ret))
    return(ret)
}
        

#' @export cumsum.mlydata
'cumsum.mlydata' <- function (x, ...) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    ret <- 'cumsum'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', class(ret))
    return(ret)
}
        

#' @export diff.zoocat
'diff.zoocat' <- function (x, ...) {
    tcolAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- 'diff'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', class(ret))
    return(ret)
}
        

#' @export diff.mlydata
'diff.mlydata' <- function (x, ...) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    ret <- 'diff'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', class(ret))
    return(ret)
}
        

#' @export head.zoocat
'head.zoocat' <- function (x, ...) {
    tcolAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- 'head'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', class(ret))
    return(ret)
}
        

#' @export head.mlydata
'head.mlydata' <- function (x, ...) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    ret <- 'head'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', class(ret))
    return(ret)
}
        

#' @export lag.zoocat
'lag.zoocat' <- function (x, ...) {
    tcolAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- 'lag'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', class(ret))
    return(ret)
}
        

#' @export lag.mlydata
'lag.mlydata' <- function (x, ...) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    ret <- 'lag'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', class(ret))
    return(ret)
}
        

#' @export na.aggregate.zoocat
'na.aggregate.zoocat' <- function (x, ...) {
    tcolAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- 'na.aggregate'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', class(ret))
    return(ret)
}
        

#' @export na.aggregate.mlydata
'na.aggregate.mlydata' <- function (x, ...) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    ret <- 'na.aggregate'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', class(ret))
    return(ret)
}
        

#' @export na.approx.zoocat
'na.approx.zoocat' <- function (x, ...) {
    tcolAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- 'na.approx'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', class(ret))
    return(ret)
}
        

#' @export na.approx.mlydata
'na.approx.mlydata' <- function (x, ...) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    ret <- 'na.approx'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', class(ret))
    return(ret)
}
        

#' @export na.contiguous.zoocat
'na.contiguous.zoocat' <- function (x, ...) {
    tcolAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- 'na.contiguous'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', class(ret))
    return(ret)
}
        

#' @export na.contiguous.mlydata
'na.contiguous.mlydata' <- function (x, ...) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    ret <- 'na.contiguous'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', class(ret))
    return(ret)
}
        

#' @export na.fill.zoocat
'na.fill.zoocat' <- function (x, ...) {
    tcolAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- 'na.fill'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', class(ret))
    return(ret)
}
        

#' @export na.fill.mlydata
'na.fill.mlydata' <- function (x, ...) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    ret <- 'na.fill'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', class(ret))
    return(ret)
}
        

#' @export na.spline.zoocat
'na.spline.zoocat' <- function (x, ...) {
    tcolAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- 'na.spline'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', class(ret))
    return(ret)
}
        

#' @export na.spline.mlydata
'na.spline.mlydata' <- function (x, ...) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    ret <- 'na.spline'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', class(ret))
    return(ret)
}
        

#' @export na.trim.zoocat
'na.trim.zoocat' <- function (x, ...) {
    tcolAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- 'na.trim'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', class(ret))
    return(ret)
}
        

#' @export na.trim.mlydata
'na.trim.mlydata' <- function (x, ...) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    ret <- 'na.trim'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', class(ret))
    return(ret)
}
        

#' @export rollapply.zoocat
'rollapply.zoocat' <- function (x, ...) {
    tcolAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- 'rollapply'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', class(ret))
    return(ret)
}
        

#' @export rollapply.mlydata
'rollapply.mlydata' <- function (x, ...) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    ret <- 'rollapply'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', class(ret))
    return(ret)
}
        

#' @export rollmax.zoocat
'rollmax.zoocat' <- function (x, ...) {
    tcolAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- 'rollmax'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', class(ret))
    return(ret)
}
        

#' @export rollmax.mlydata
'rollmax.mlydata' <- function (x, ...) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    ret <- 'rollmax'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', class(ret))
    return(ret)
}
        

#' @export rollmean.zoocat
'rollmean.zoocat' <- function (x, ...) {
    tcolAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- 'rollmean'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', class(ret))
    return(ret)
}
        

#' @export rollmean.mlydata
'rollmean.mlydata' <- function (x, ...) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    ret <- 'rollmean'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', class(ret))
    return(ret)
}
        

#' @export rollmedian.zoocat
'rollmedian.zoocat' <- function (x, ...) {
    tcolAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- 'rollmedian'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', class(ret))
    return(ret)
}
        

#' @export rollmedian.mlydata
'rollmedian.mlydata' <- function (x, ...) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    ret <- 'rollmedian'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', class(ret))
    return(ret)
}
        

#' @export rollsum.zoocat
'rollsum.zoocat' <- function (x, ...) {
    tcolAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- 'rollsum'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', class(ret))
    return(ret)
}
        

#' @export rollsum.mlydata
'rollsum.mlydata' <- function (x, ...) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    ret <- 'rollsum'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', class(ret))
    return(ret)
}
        

#' @export tail.zoocat
'tail.zoocat' <- function (x, ...) {
    tcolAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- 'tail'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', class(ret))
    return(ret)
}
        

#' @export tail.mlydata
'tail.mlydata' <- function (x, ...) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    ret <- 'tail'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', class(ret))
    return(ret)
}
        

#' @export coredata<-.zoocat
'coredata<-.zoocat' <- function (x, ...) {
    tcolAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- 'coredata<-'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', class(ret))
    return(ret)
}
        

#' @export coredata<-.mlydata
'coredata<-.mlydata' <- function (x, ...) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    ret <- 'coredata<-'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', class(ret))
    return(ret)
}
        

#' @export index<-.zoocat
'index<-.zoocat' <- function (x, ...) {
    tcolAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- 'index<-'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', class(ret))
    return(ret)
}
        

#' @export index<-.mlydata
'index<-.mlydata' <- function (x, ...) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    ret <- 'index<-'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', class(ret))
    return(ret)
}
        
