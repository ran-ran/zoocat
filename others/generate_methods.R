fout <- 'E:/Codes/zoocat/R/all_others.R'

if (file.exists(fout)) {
    file.remove(fout)
}

fcon <- file(fout, 'w')

methodNames_1 <- c('cummax',
                 'cummin',
                 'cumprod',
                 'cumsum',
                 'diff',
                 'head',
                 'lag',
                 'na.aggregate', 
                 'na.approx',
                 'na.contiguous',
                 'na.fill',
                 'na.spline',
                 'na.trim',
                 'rollapply',
                 'rollmax',
                 'rollmean',
                 'rollmedian',
                 'rollsum',
                 'scale',
                 'tail',
                 'coredata<-',
                 'index<-'
                 )

methodNames_2 <- c('as.data.frame',
                   'as.matrix',
                   'coredata',
                   'barplot'
                   )

for (mtd in methodNames_1) {
    cat(
"
#' @export 
'", mtd, ".zoocat' <- function (x, ...) {
    colAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- '",
    mtd, "'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', class(ret))
    return(ret)
}
        
",

"
#' @export
'", mtd, ".mlydata' <- function (x, ...) {
    month <- attr(x, 'month')
    x <- as.zoo(x)
    ret <- '",
    mtd, "'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'month') <- month 
    class(ret) <- c('mlydata', class(ret))
    return(ret)
}
        
",
        sep = '',
        file = fcon)
}




for (mtd in methodNames_2) {
    cat(
"
#' @export 
'", mtd, ".zoocat' <- function (x, ...) {
    x <- as.zoo(x)
    ret <- '",
    mtd, "'(x, ...)
    return(ret)
}
        
",

"
#' @export
'", mtd, ".mlydata' <- function (x, ...) {
    x <- as.zoo(x)
    ret <- '",
    mtd, "'(x, ...)
    return(ret)
}
        
",
        sep = '',
        file = fcon)
}



close(fcon)
    
        
        
            
        
        
