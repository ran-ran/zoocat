fout <- 'E:/Codes/zoocat/R/all_others.R'

if (file.exists(fout)) {
    file.remove(fout)
}

fcon <- file(fout, 'w')

methodNames <- c('cummax',
                 'cummin',
                 'cumprod',
                 'cumsum',
                 'diff',
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
                 'coredata<-'
                 )

for (mtd in methodNames) {
    cat(
"
#' @export
'", mtd, ".zoocat' <- function (x, ...) {
    tcolAttr <- cattr(x)
    x <- as.zoo(x)
    ret <- '",
    mtd, "'(x, ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    class(ret) <- c('zoocat', 'zoo')
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
    class(ret) <- c('mlydata', 'zoo')
    return(ret)
}
        
",
        sep = '',
        file = fcon)
}
close(fcon)
    
        
        
            
        
        
