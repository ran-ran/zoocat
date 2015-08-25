
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
#' means an attribute field of the columns.\cr \code{isvalid} can be used to
#' determine whether the object is a legal object. If it is not legal, some
#' information will be showed.\cr When printing object, column names will be
#' added automatically.\cr \code{coredata} can be used to extract the core data
#' matrix or data frame from the object.\cr \code{as.matrix} returns a matrix
#' with column and row names. The row names is the index of the \code{zoocat}
#' object, and the column names is based on the \code{cattr}.
#' 
#' @param x For \code{zoocat} function, x is a matrix or a data frame.
#' Otherwise, x is a \code{zoocat} object.
#' @return \code{zoocat} returns a \code{zoocat} object. \code{isvalid} returns
#' a logical variable. \code{coredata} and \code{as.matrix} returns a matrix or
#' a data frame.
#' @examples
#' 
#' x <- matrix(1 : 20, nrow = 5)
#' colAttr <- data.frame(month = c(2, 3, 5, 6), name = 'sst')
#' zc <- zoocat(x, order.by = 1991 : 1995, colattr = colAttr)
#' unclass(zc)
#' zc[1, 3]
#' zc[2, ]
#' coredata(zc)
#' as.matrix(zc)
#' 
#' @name zoocat
#' @export
#' @rdname zoocat
zoocat <- function (x = NULL, order.by = NULL, colattr = NULL) {
    if (is.null(x) & is.null(order.by) & is.null(colattr)) {
        z <- rep(0, 0)
        class(z) <- c('zoocat', 'zoo')
        return(z)
    }
    stopifnot(class(x) == 'matrix' | class(x) == 'data.frame')
    stopifnot(class(colattr) == 'data.frame' | class(colattr) == 'matrix')
    if (is.data.frame(colattr)) {
        for (i in 1 : ncol(colattr)) {
            if (is.factor(colattr[, i])) {
                colattr[, i] <- as.character(colattr[, i])
            }
        }
    }
    stopifnot(nrow(colattr) == ncol(x))
    stopifnot(!is.null(colnames(colattr)))
    colnames(x) <- NULL
    rownames(x) <- NULL
    z <- zoo(x, order.by = order.by)
    rownames(colattr) <- NULL
    attr(z, 'cattr') <- colattr
    class(z) <- c('zoocat', 'zoo')
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
        colnames(x) <- cattr2name(attr(x, 'cattr'))
        attr(x, 'cattr') <- NULL
        cat('A zoocat object with column attributes: ')
        for (i in 1 : length(attrName)) {
            cat(attrName[i])
            if (i < length(attrName)) {
                cat('/ ')
            }
        }
        cat('\n\n')
        print(x)
    }
}


#' @export
#' @rdname zoocat
'[.zoocat' <- function(x, i = NULL, j = NULL, ...) {
    if (length(x) == 0) {
        return(zoocat())
    }
    if (is.null(i)) {
        i <- 1 : nrow(x)
    }
    if (is.null(j)) {
        j <- 1 : ncol(x)
    }
    if (is.character(i) | is.character(j)) {
        stop('i and j must be numeric or integer.')
    }
    colAttr <- attr(x, 'cattr')
    class(x) <- 'zoo'
    x <- x[i, j, drop = FALSE]
    attr(x, 'cattr') <- colAttr[j, , drop = FALSE]
    class(x) <- c('zoocat', 'zoo')
    return(x)
}




#' @export
#' @rdname zoocat
as.matrix.zoocat <- function (x) {
    if (length(x) == 0) {
        return(c())
    }
    class(x) <- NULL
    rownames(x) <- attr(x, 'index')
    colnames(x) <- cattr2name(attr(x, 'cattr'))
    attr(x, 'cattr') <- NULL
    attr(x, 'index') <- NULL
    return(x)
}



#' @export
#' @rdname zoocat
coredata.zoocat <- function (x) {
    if (length(x) == 0) {
        return(c())
    }
    attr(x, 'cattr') <- NULL
    attr(x, 'index') <- NULL
    colnames(x) <- NULL
    rownames(x) <- NULL
    class(x) <- NULL
    return(x)
}

