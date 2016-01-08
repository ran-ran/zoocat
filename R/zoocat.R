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
#' @param x a matrix. If \code{x} is a data frame, it will be converted to a matrix.
#' @param colattr the column attributes. Must be a data frame with column names.
#' @param index.name. the name of the index variable.
#' @param ... other arguments for \code{zoo}.
#' @return A \code{zoocat} object.
#' @examples
#' 
#' x <- matrix(1 : 20, nrow = 5)
#' colAttr <- data.frame(month = c(2, 3, 5, 6), name = c(rep('xxx', 3), 'yyy'))
#' zc <- zoocat(x, order.by = 1991 : 1995, colattr = colAttr)
#' unclass(zc)
#' zc[1, 3]
#' zc[2, ]
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
zoocat <- function (x = NULL, colattr = NULL, index.name. = 'index', ...) {
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
    attr(z, 'index.name') <- index.name
    class(z) <- c('zoocat', class(z))
    return(z)
}


#' @export
print.zoocat <- function (x, ...) {
    if (length(x) == 0) {
        cat('empty zoocat\n')
    } else {
        attrName <- colnames(cattr(x))
        colnames(x) <- cattr2str(attr(x, 'cattr'))
        cat('A zoocat object with:\n- [column attributes]: ')
        for (i in 1 : length(attrName)) {
            cat(attrName[i])
            if (i < length(attrName)) {
                cat(', ')
            }
        }
        cat('\n- [index variable]: ', attr(x, 'index.name'), sep = '')
        cat('\n\n')
        class(x) <- 'zoo'
        attr(x, 'cattr') <- NULL
        attr(x, 'index.name') <- NULL
        print(x)
    }
}


#' @export
'[.zoocat' <- function(x, i, j, drop = TRUE, ...) {
    if (length(x) == 0) {
        return(zoocat())
    }
    if (missing(i)) {
        i <- 1 : nrow(x)
    }
    if (missing(j)) {
        j <- 1 : ncol(x)
    }
    if (inherits(x, 'zoomly')) {
        fun_cattr2str <- month2str
    } else {
        fun_cattr2str <- cattr2str
    }
    class0 <- class(x)
    colAttr <- attr(x, 'cattr')
    
    if (is.character(j)) {
        cattrStr <- fun_cattr2str(colAttr)
        j <- sapply(j, FUN = function (j1) which(j1 == cattrStr)[1])
        if (any(is.na(j))) {
            stop('Some column does not exist.')
        }
    }
    colAttr <- colAttr[j, , drop = FALSE]
    class(x) <- class(x)[class(x) %in% c('zooreg', 'zoo')]
    x <- x[i, j, drop = drop]
    
    if (drop == TRUE & length(i) == 1) {
        x <- as.vector(x)
        if (class0[1] == 'zoomly') {
            names(x) <- fun_cattr2str(colAttr)
        } else {
            names(x) <- cattr2str(colAttr)
        }
    } else if (drop == FALSE | (length(i) > 1 & length(j) > 1)) { 
        attr(x, 'cattr') <- colAttr
        class(x) <- class0
    }
    
    return(x)
}




