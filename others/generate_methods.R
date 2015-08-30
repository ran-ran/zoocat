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
                 'tail',
                 'coredata<-',
                 'index<-'
                 )

for (mtd in methodNames) {
    cat(
"
#' @export ", mtd, ".zoocat
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
#' @export ", mtd, ".mlydata
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
close(fcon)
    
        
        
            
        
        
