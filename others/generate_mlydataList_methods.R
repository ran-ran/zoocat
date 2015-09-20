
fout <- 'E:/Codes/zoocat/R/z_mlydataList_methods.R'

if (file.exists(fout)) {
    file.remove(fout)
}

fcon <- file(fout, 'w')

methodNames <- matrix(
                 c(
                 'window', 'x', 'window',
                 'expand', 'x', 'expand', 
                 'normalize', 'x', 'normalize',
                 'cummin', 'x', '',
                 'cumprod', 'x', '',
                 'cumsum', 'x', '',
                 'diff', 'x', '', 
                 'head', 'x', '',
                 'lag', 'x', '',
                 'na.aggregate', 'object', '',
                 'na.approx', 'object', '',
                 'na.contiguous', 'object', '',
                 'na.fill', 'object', '',
                 'na.spline', 'object', '',
                 'na.trim', 'object', '',
                 'rollapply', 'data', '',
                 'rollmax', 'x', '',
                 'rollmean', 'x', '',
                 'rollmedian', 'x', '',
                 'rollsum', 'x', '',
                 'scale', 'x', '',
                 'tail', 'x', ''
                 ),
                 ncol = 3, byrow = TRUE)


for (i in 1 : nrow(methodNames)) {
    mtd <- methodNames[i, 1]
    obj <- methodNames[i, 2]
    rdname <- methodNames[i, 3]
    
    cat("
#' @export 
", 
        sep = '',
        file = fcon)

if (nchar(rdname) != 0) {
    cat("#' @rdname ", rdname, "\n",
        sep = '',
        file = fcon)
}

    cat(
"'", mtd, ".mlydataList' <- function (", obj, ", ...) {
    for (i in 1 : length(", obj, ")) {
        ", obj, "[[i]] <- ", mtd, "(", obj, "[[i]], ...)
    }
    return(", obj, ")
}

",

        sep = '',
        file = fcon)
}


close(fcon)
