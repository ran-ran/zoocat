#'
#' zoocat Class
#' 
#' A class designed for \code{zoo} with column attributes.
#' 
#' \code{zoocat} is a class based on the \code{zoo} class, which means
#' \code{zoo} with column(C) attributes(AT). An attribute named "cattr" is
#' added in the object, so there are two important attributes in the object:
#' \code{index} and \code{cattr}. \code{index} is inherited from \code{zoo}
#' class, which keeps the row's index. \code{cattr}, which is a data.frame of
#' matrix, keeps the attribute informations of columns. The row number of
#' \code{cattr} must be the same with the number of columns of the data matrix,
#' and each row of \code{cattr} is correspondent to each column of the data
#' matrix. \code{cattr} must have column names, and each column of \code{cattr}
#' means an attribute field of the columns.\cr
#' In summary, zoocat class can manage the informations of each column more 
#' conveniently than only using column names.
#' \cr When printing object, column names will be
#' added automatically.\cr \code{coredata} can be used to extract the core data
#' matrix or data frame from the object.\cr \code{as.matrix} returns a matrix
#' with column and row names. The row names is the index of the \code{zoocat}
#' object, and the column names is based on the \code{cattr}.
#' 
#' @param x For \code{zoocat} function, x is a matrix or a data frame.
#' Otherwise, x is a \code{zoocat} object.
#' @param colattr The column attributes. Must be a data frame with column names.
#' @param ... Other arguments for \code{zoo}.
#' @return \code{zoocat} returns a \code{zoocat} object. \code{coredata} and \code{as.matrix} returns a matrix or
#' a data frame.
#' @examples
#' 
#' x <- matrix(1 : 20, nrow = 5)
#' colAttr <- data.frame(month = c(2, 3, 5, 6), name = c(rep('xxx', 3), 'yyy'))
#' zc <- zoocat(x, order.by = 1991 : 1995, colattr = colAttr)
#' unclass(zc)
#' zc[1, 3]
#' zc[1, 3, drop = T]
#' zc[2, ]
#' zc[2, , drop = T]
#' zc[c(T, F, F, T, T), ]
#' zc[, c(T, F, F, T)]
#' zc[, '2_xxx']
#' coredata(zc)
#' as.matrix(zc)
#' 
#' x <- matrix(1 : 20, nrow = 5)
#' colAttr <- data.frame(month = c(2, 3, 5, 6), name = c(rep('xxx', 3), 'yyy'))
#' zc <- zoocat(x, order.by = 1991 : 1995, colattr = colAttr, frequency = 1)
#' 
#' @name zoocat
#' @export
#' @rdname zoocat
zoocat <- function (x = NULL, colattr = NULL, ...) {
    if (is.null(x)) {
        z <- zoo(x, ...)
        class(z) <- c('zoocat', class(z))
        return(z)
    }
    stopifnot(is.matrix(x) | is.data.frame(x))
    stopifnot(class(colattr) == 'data.frame')
    stopifnot(nrow(colattr) == ncol(x))
    stopifnot(!is.null(colnames(colattr)))
    colnames(x) <- NULL
    rownames(x) <- NULL
    z <- zoo(x, ...)
    rownames(colattr) <- NULL
    attr(z, 'cattr') <- colattr
    class(z) <- c('zoocat', class(z))
    return(z)
}


#' @export
#' @rdname zoocat
print.zoocat <- function (x) {
    if (length(x) == 0) {
        cat('empty zoocat\n')
    } else {
        attrName <- colnames(cattr(x))
        class(x) <- 'zoo'
        colnames(x) <- cattr2str(attr(x, 'cattr'))
        attr(x, 'cattr') <- NULL
        cat('A zoocat object with column attributes: ')
        for (i in 1 : length(attrName)) {
            cat(attrName[i])
            if (i < length(attrName)) {
                cat('_')
            }
        }
        cat('\n\n')
        print(x)
    }
}


#' @export
#' @rdname zoocat
'[.zoocat' <- function(x, i = NULL, j = NULL, drop = FALSE) {
    if (length(x) == 0) {
        return(zoocat())
    }
    if (is.null(i)) {
        i <- 1 : nrow(x)
    }
    if (is.null(j)) {
        j <- 1 : ncol(x)
    }
    colAttr <- attr(x, 'cattr')
    x <- as.zoo(x)
    x <- x[i, j, drop = FALSE]
    
    if (is.character(j)) {
        j <- which(j %in% cattr2str(colAttr))
    }
    attr(x, 'cattr') <- colAttr[j, , drop = FALSE]
    class(x) <- c('zoocat', class(x))
    
    if (drop == TRUE) {
        if (nrow(x) == 1) {
            cname <- colnames(x)
            x <- as.vector(x)
            names(x) <- cname
        } else if (ncol(x) == 1) {
            rname <- index(x)
            x <- as.vector(x)    
            names(x) <- rname
        }
    }
    
    colnames(x) <- NULL
    return(x)
}




