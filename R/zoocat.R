#'
#' \code{zoocat} class
#' 
#' A class designed for "\code{zoo}" class with a column attribute (\bold{cattr}) table.
#' 
#' 
#' "\code{zoocat}" is a S3 class based on the "\code{zoo}" class, which means
#' "\code{zoo}" with column(C) attributes(AT). 
#' It is known that a "\code{zoo}" object is a vector or matrix with a index attribute.
#' In a "\code{zoocat}" object, another attribute named "\bold{cattr}" 
#' (a data frame with column names) 
#' is added to keeps the underlying attribute information of each column.
#' So there are two attributes in a "\code{zoocat}" object: "\code{index}" 
#' and "\code{cattr}".
#' The number of rows of the "\code{cattr}" table must be the same with \code{ncol(x)},
#' where \code{x} is the core data.
#' Each row in the "\code{cattr}" table is correspondent to each column of the 
#' core data matrix.
#' 
#' In summary, "\code{zoocat}" class can manage the underlying information 
#' of each column more conveniently than only using column names.
#' It can be used to store time series data each column of which is corresponding to 
#' several underlying variables.
#' 
#' Two methods can be used to build a "\code{zoocat}" object.
#' The first is to use function \code{zoocat}, the "\code{cattr}" table is specified
#' by argument \code{colattr}.
#' The second method is to use \code{\link{cast2zoocat}} to get the object from a 
#' data frame.
#' 
#' When printing "\code{zoocat}" objects, column names will be added automatically, 
#' but it should be noted that the column names do not exist. 
#' As "\code{zoo}" object, \code{\link[zoo]{coredata}} can be used to 
#' extract the core data matrix from the object. 
#' 
#' Many methods have been defined for the "\code{zoocat}" object.
#' \code{\link{filter_col}} can be used to get columns which satisfy some conditions.
#' \code{\link{melt}} can be used to melt the object, like what 
#' \code{\link[reshape2]{melt}} in the \pkg{reshape2} do.
#' \code{\link{normalize}} can be used to normalized data using several methods.
#' \code{\link{apply_col}} can be used to apply a function to each column and bind the 
#' results with the "\code{cattr}" table.
#' \code{\link{apply_core}} can apply a function to the whole core data matrix, and bind
#' the results with the "\code{index}" or "\code{cattr}" table.
#' 
#' It should be noted that all methods for "\code{zoo}" objects can be used for 
#' "\code{zoocat}" objects, such as \code{na.omit}, 
#' \code{\link[zoo]{na.approx}}, \code{\link[zoo]{na.fill}}, 
#' \code{\link[zoo]{na.trim}}, \code{lag}.
#' See the help page of \code{\link[zoo]{zoo}}.
#' 
#' 
#' 
#' @param x a matrix. If \code{x} is a data frame, it will be converted to a matrix.
#' @param order.by an index vector with unique entries by which the observations
#'  in x are ordered.
#' @param colattr the column attributes. Must be a data frame with column names.
#' @param index.name the name of the index variable.
#' @param ... other arguments for \code{zoo}.
#' @return A "\code{zoocat}" object.
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
zoocat <- function (x = NULL, order.by = index(x), colattr = NULL, index.name = 'index', ...) {
    if (is.null(x)) {
        z <- zoo(x, ...)
        class(z) <- c('zoocat', class(z))
        return(z)
    }
    if (is.data.frame(x)) {
        x <- as.matrix(x)
    }
    if (!is.matrix(x)) {
        stop('x must be a matrix.')
    }
    if (!is.data.frame(colattr)) {
        stop('colattr must be a data frame.')
    }
    if (nrow(colattr) != ncol(x)) {
        stop('nrow(colattr) and ncol(x) must be the same.')
    }
    if (is.null(colnames(colattr))) {
        stop('colattr must have column names.')
    }
    
    colnames(x) <- NULL
    rownames(x) <- NULL
    z <- zoo(x, order.by = order.by, ...)
    rownames(colattr) <- NULL
    attr(z, 'cattr') <- colattr
    attr(z, 'indname') <- index.name
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
        cat('A zoocat object with:\n- [column attribute fields]: ')
        for (i in 1 : length(attrName)) {
            cat(attrName[i])
            if (i < length(attrName)) {
                cat(', ')
            }
        }
        cat('\n- [index variable]: ', attr(x, 'indname'), sep = '')
        cat('\n- [data]:\n')
        class(x) <- 'zoo'
        attr(x, 'cattr') <- NULL
        attr(x, 'indname') <- NULL
        print(x)
    }
}



coredata.zoocat <- function (x, ...) {
    attr(x, "class") <- NULL
    attr(x, "cattr") <- NULL
    attr(x, "indname") <- NULL
    attr(x, "index") <- NULL
    attr(x, "frequency") <- NULL
    return(x) 
}


#' @export 
'index<-.zoocat' <- function (x, value) {
    if (length(index(x)) != length(value)) {
        stop("length of index vectors does not match.")
    }
    if (is.unsorted(ORDER(value))) {
        stop("new index needs to be sorted.")
    }
    attr(x, 'index') <- value
    return(x)
}


