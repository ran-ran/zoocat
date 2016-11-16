

#' Apply a function over the core data matrix
#' 
#' Apply a function over the core data matrix of the "\code{zoocat}" object,
#' and bind the returned data with \code{cattr} or \code{index}.
#' 
#' If \code{FUN} return a vector, \code{bind} can be one of:
#' \enumerate{
#' \item "cattr": In this case, the vector will be combined with the \code{cattr} 
#' to return a data frame.
#' \item "index": In this case, the vector will be combined with the \code{index} 
#' to return a "\code{zoo}" object.
#' }
#' 
#' If \code{FUN} return a matrix named \code{x.ret}, \code{bind} can be one of: 
#' \enumerate{
#' \item c("index", "cattr"): In this case, the rows of \code{x.ret}
#' will be conbined with the \code{index}, and the columns of \code{x.ret} will be combined 
#' with the \code{cattr}. So a "\code{zoocat}" object will be returned.
#' \item c("cattr", "index"): In this case, the rows of \code{x.ret} will be combined 
#' with the \code{cattr}, and the columns of \code{x.ret} will be combined with the \code{index}.
#' So a "\code{zoocat}" object will be returned, but the core data in the "\code{zoocat}" object will 
#' be \code{t(x.ret)}.
#' \item c("cattr", NA): In this case, the rows of \code{x.ret} will be combined 
#' with the \code{cattr} to return a data frame.
#' \item c(NA, "cattr"): In this case, the columns of \code{x.ret} will be combined 
#' with the \code{cattr} to return a data frame.
#' \item c("index", NA): In this case, the rows of \code{x.ret} will be combined 
#' with the \code{index} to return a "\code{zoo}" object.
#' \item c(NA, "index"): In this case, the columns of \code{x.ret} will be combined 
#' with the \code{index} to return a "\code{zoo}" object.
#' }
#' 
#' Note that if \code{bind} is \code{NULL} (default), the original returned value of \code{FUN} will be returned.
#' 
#' @export
#' @name apply_core
#' @rdname apply_core
apply_core <- function (x, ...) {
    UseMethod('apply_core')
}

#' @export
#' @return a data frame, a "\code{zoo}" object, or a "\code{zoocat}" object. 
#' @rdname apply_core
#' @param x the object.
#' @param FUN the function to apply. The \code{FUN} must return a matrix or 
#' a vector.
#' @param bind a vector of length 1 or 2 with element values to be
#'  'cattr' or 'index' or NA to describe how to bind the returned
#' data with \code{cattr} or \code{index}. If \code{FUN} return a vector, set
#' \code{bind} to be a scalar. If \code{FUN} return a matrix, set
#' \code{bind} to be a vector of length 2. See details.
#' @param ... other arguments for \code{FUN}.
#' @examples 
#' x <- matrix(1 : 20, nrow = 5)
#' colAttr <- data.frame(month = c(2, 3, 5, 6), name = c(rep('xxx', 3), 'yyy'))
#' zc <- zoocat(x, order.by = 1991 : 1995, colattr = colAttr)
#' 
#' apply_core(zc, FUN = colMeans)
#' apply_core(zc, FUN = colMeans, bind = 'cattr')
#' 
#' apply_core(zc, FUN = rowMeans)
#' apply_core(zc, FUN = rowMeans, bind = 'index')
#' 
#' apply_core(zc, FUN = function (x) {x*2})
#' apply_core(zc, FUN = function (x) {x*2}, bind = c('index', 'cattr'))
#' apply_core(zc, FUN = function (x) {t(x*2)}, bind = c('cattr', 'index'))
#' apply_core(zc, FUN = function (x) {x*2}, bind = c('index', NA))
#' apply_core(zc, FUN = function (x) {x[3:4, ]}, bind = c(NA, 'cattr'))
#' apply_core(zc, FUN = function (x) 
#'                       {r <- x[3:4, ]
#'                       rownames(r) <- c('a', 'b')
#'                       return(r)}, 
#'                       bind = c(NA, 'cattr'))
#' 
#' vec <- as.vector(zc[, 1])
#' apply_core(zc, FUN = function (x) {as.vector(cor(x, vec))}, bind = 'cattr')
#' 
apply_core.zoocat <- function (x, FUN, bind = NULL, ...) {
    if (!is.null(bind)) {
        if (!all(bind %in% c('cattr', 'index', NA))) {
            stop('elements of bind must be "cattr", "index" or NA')
        }
        if (!(length(bind) %in% c(1, 2))) {
            stop('the length of bind must be 1 or 2.')
        }
    }
    
    core.mat <- coredata(x)
    if (!is.matrix(core.mat)) {
        core.mat <- as.matrix(core.mat)
    }
    data.ret <- FUN(core.mat, ...)
    
    if (is.null(bind)) {
        return(data.ret)
    }
    
    if (is.data.frame(data.ret)) {
        data.ret <- as.matrix(data.ret)
    }
    if (!is.matrix(data.ret) & !is.vector(data.ret)) {
        stop('FUN must return a matrix or a vector.')
    }
    
    if (is.vector(data.ret)) {
        if (length(bind) != 1 | any(is.na(bind))) {
           stop('Because FUN return a vector, bind must be a scalar and the 
                value should not be NA.') 
        }
        if (bind == 'cattr') {
           if (ncol(x) != length(data.ret)) {
               stop('the size of the returned matrix is not proper.')
           }
           ret <- cbind(cattr(x), output = data.ret)
        } else if (bind == 'index') {
           if (nrow(x) != length(data.ret)) {
               stop('the size of the returned matrix is not proper.')
           }
           ret <- zoo(data.ret, order.by = index(x))
        } else {
           stop('bind is incorrect.')
        }
    }
    
    if (is.matrix(data.ret)) {
        if (length(bind) != 2) {
           stop('Because FUN return a matrix, bind must be a vector with two elements.') 
        }
        if (all(is.na(bind))) {
           stop('bind is incorrect for all values is NA.')
        }
        if (identical(bind, c('index', 'cattr'))) {
            if (nrow(data.ret) != nrow(x) | ncol(data.ret) != ncol(x)) {
                stop('the size of the returned matrix is not proper.')
            }
            ret <- zoocat(data.ret, order.by = index(x), colattr = cattr(x))
        } else if (identical(bind, c('cattr', 'index'))) {
            if (nrow(data.ret) != ncol(x) | ncol(data.ret) != nrow(x)) {
                stop('the size of the returned matrix is not proper.')
            }
            ret <- zoocat(t(data.ret), order.by = index(x), colattr = cattr(x))
        } else if (identical(bind, c('cattr', NA))) {
            if (nrow(data.ret) != ncol(x)) {
                stop('the size of the returned matrix is not proper.')
            }
            if (is.null(colnames(data.ret))) {
                colnames(data.ret) <- paste('output', 1 : ncol(data.ret), sep = '')
            }
            ret <- cbind(cattr(x), data.ret)
        } else if (identical(bind, c(NA, 'cattr'))) {
            if (ncol(data.ret) != ncol(x)) {
                stop('the size of the returned matrix is not proper.')
            }
            if (is.null(rownames(data.ret))) {
                rownames(data.ret) <- paste('output', 1 : nrow(data.ret), sep = '')
            }
            ret <- cbind(cattr(x), t(data.ret))
        } else if (identical(bind, c('index', NA))) {
            if (nrow(data.ret) != nrow(x)) {
                stop('the size of the returned matrix is not proper.')
            }
            if (is.null(colnames(data.ret))) {
                colnames(data.ret) <- paste('output', 1 : ncol(data.ret), sep = '')
            }
            ret <- zoo(data.ret, order.by = index(x))
        } else if (identical(bind, c(NA, 'index'))) {
            if (ncol(data.ret) != nrow(x)) {
                stop('the size of the returned matrix is not proper.')
            }
            if (is.null(rownames(data.ret))) {
                rownames(data.ret) <- paste('output', 1 : nrow(data.ret), sep = '')
            }
            ret <- zoo(t(data.ret), order.by = index(x))
        } 
        else {
            stop('bind is incorrect.')
        }
    }
    
    return(ret)
}









