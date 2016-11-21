fout <- 'D:/HeranFiles/worknow/MyPkgs/zoocat/R/zoocat_other_methods.R'

if (file.exists(fout)) {
    file.remove(fout)
}

fcon <- file(fout, 'w')


# These methods will return a object of the same class as 
# the input.
methodNames_1 <- matrix(
                 c(
                 'cummax', 'x',
                 'cummin', 'x',
                 'cumprod', 'x',
                 'cumsum', 'x',
                 'diff', 'x',
                 'head', 'x',
                 'lag', 'x',
                 'na.aggregate', 'object',
                 'na.approx', 'object',
                 'na.contiguous', 'object',
                 'na.fill', 'object',
                 'na.spline', 'object',
                 'na.trim', 'object',
                 'rollapply', 'data',
                 'rollmax', 'x',
                 'rollmean', 'x',
                 'rollmedian', 'x',
                 'rollsum', 'x',
                 'scale', 'x',
                 'tail', 'x',
                 'window', 'x'
                 ),
                 ncol = 2, byrow = TRUE)


# These methods do not need to consider the class of return.
methodNames_2 <- matrix( 
                c(
                    'as.matrix', 'x',
                    'as.data.frame', 'x',
                    'plot', 'x',
                    'barplot', 'height'
                   ),
                 ncol = 2, byrow = TRUE)

# Replace methods.
methodNames_3 <- matrix(nrow = 0, ncol = 2, byrow = TRUE)
                


##############################################
# methods definition
##############################################

for (i in 1 : nrow(methodNames_1)) {
    mtd <- methodNames_1[i, 1]
    obj <- methodNames_1[i, 2]
    cat(
"
#' @export 
'", mtd, ".zoocat' <- function (", obj, ", ...) {
    class0 <- class(", obj, ")
    colAttr <- cattr(", obj, ")
    indexName <- indname(", obj, ")
    ", obj, " <- as.zoo(", obj, ", add.colname = FALSE)
    ret <- '", mtd, "'(", obj, ", ...)
    colnames(ret) <- NULL
    attr(ret, 'cattr') <- colAttr
    attr(ret, 'indname') <- indexName
    class(ret) <- class0
    return(ret)
}
        
",

        sep = '',
        file = fcon)
}




for (i in 1 : nrow(methodNames_2)) {
    mtd <- methodNames_2[i, 1]
    obj <- methodNames_2[i, 2]
    cat(
"
#' @export 
'", mtd, ".zoocat' <- function (", obj, ", ...) {
    ", obj, " <- as.zoo(", obj, ")
    ret <- '", mtd, "'(", obj, ", ...)
    return(ret)
}
        
",

        sep = '',
        file = fcon)
}


if (nrow(methodNames_3) > 0) {
    for (i in 1 : nrow(methodNames_3)) {
        mtd <- methodNames_3[i, 1]
        obj <- methodNames_3[i, 2]
        cat(
    "
    #' @export 
    '", mtd, ".zoocat' <- function (", obj, ", value) {
        class0 <- class(", obj, ")
        colAttr <- cattr(", obj, ")
        ", obj, " <- as.zoo(", obj, ", add.colname = FALSE)
        ret <- '", mtd, "'(", obj, ", value)
        colnames(ret) <- NULL
        attr(ret, 'cattr') <- colAttr
        class(ret) <- class0
        return(ret)
    }
            
    ",
    
            sep = '',
            file = fcon)
    }
}


close(fcon)
    
        
        
            
        
        
