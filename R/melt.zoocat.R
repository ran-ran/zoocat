
#' Melt a \code{zoocat} Object
#' 
#' Melt a \code{zoocat} object, which is similar as in package \code{reshape2}.
#' 
#' @param x A \code{zoocat} object.
#' @param value.name Name of variable used to store values.
#' @param index.name Name of variable used to store the index of the \code{zoocat} object.
#' 
#' @return A data frame.
#' 
#' @export
#' @examples
#' 
#' x <- matrix(1 : 20, nrow = 5)
#' colAttr <- data.frame(month = c(2, 3, 5, 6), name = c('x', 'y', 'z', 'a'))
#' zc <- zoocat(x, order.by = 1991 : 1995, colattr = colAttr)
#' melt(zc)
#' 
#' 
melt.zoocat <- function (x, value.name = 'value', index.name = 'index') {
    colattr <- cattr(x)
    idvars <- colnames(colattr)
    xcore <- t(as.matrix(x))
    ind <- index(x)
    ind.class <- class(ind)
    dframe <- data.frame(colattr, xcore)
    colnames(dframe) <- c(idvars, ind)
    df.melt <- melt(dframe, id.vars = idvars, variable.name = index.name, value.name = value.name,
                    factorAsStrings = FALSE)
    df.melt$index <- as.character(df.melt$index)
    class(df.melt$index) <- ind.class
    return(df.melt)
    
}