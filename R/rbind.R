
#' Merge two or more \code{zoocat} objects by rows
#' 
#' Merge two or more \code{zoocat} objects by rows. Note that all the \code{cattr} tables
#' must be the same.
#' 
#' @examples 
#' x <- matrix(1 : 20, nrow = 5)
#' colAttr <- data.frame(month = c(2, 3, 5, 6), name = c(rep('xxx', 3), 'yyy'))
#' zc <- zoocat(x, order.by = 1991 : 1995, colattr = colAttr) 
#' zc2 <- zc
#' index(zc2) <- index(zc) + 6
#' rbind(zc, zc2)
#' 
#' @param deparse.levels Not used.
#' @param ... \code{zoocat} objects.
rbind.zoocat <- function (..., deparse.levels = 1) {
    args <- list(...) 
    if (!all(sapply(args, FUN = function (x) {inherits(x, 'zoocat')}))) {
        stop('All inputs must be zoocat objects.')
    }
    if (length(args) == 1) {
        return(args[[1]])
    } 
    
    colAttr <- cattr(args[[1]])
    for (i in 2 : length(args)) {
        if (!identical(cattr(args[[i]]), colAttr)) {
            stop('All cattr table must be the same.')
        }
    }
    
    ret <- do.call(rbind.zoo, args)
    cattr(ret) <- colAttr
    return(ret)
}