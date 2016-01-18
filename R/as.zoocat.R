
#' Coercing Objects to Class \code{zoocat}
#' 
#' Coercing objects to class \code{zoocat}.
#' 
#' @return A \code{zoocat} object.
#' @examples
#' 
#' x <- matrix(1 : 20, nrow = 5)
#' zm <- zoomly(x, year = 1991 : 1995, month = c(2, 3, 5, 6))
#' zm2 <- zm + 100
#' as.zoocat(zm)
#' as.zoocat(zm, addname = FALSE)
#' as.zoocat(x = zm)
#' as.zoocat(x = zm, y = zm2)
#' as.zoocat(zm, zm2)
#' 
#' x <- zoomly(x, year = 1991 : 1995, month = c(2, 3, 5, 6))
#' y <- x + 100
#' zmList <- zoomlyList(list(x = x, y = y))
#' as.zoocat(zmList)
#' 
#' zobj <- zoo(matrix(1:10, nrow = 5), order.by = 11:15)
#' colnames(zobj) <- c('a', 'b')
#' as.zoocat(zobj)
#' 
#' @name as.zoocat
#' @rdname as.zoocat
#' @export
#' @param x the object.
#' @param ... other arguments passed to methods.
as.zoocat <- function (x, ...) { UseMethod('as.zoocat') }

#'
#' @export
#' @rdname as.zoocat
#' @param addname logical. If TRUE, a field of \code{cattr} "name" will be add.
#' @param variable.name the name of the field in the \code{cattr} of 
#' the output \code{zoocat} object to store the variable name.
#' Only valid when \code{addname} is TRUE.
as.zoocat.zoomly <- function (..., addname = TRUE, variable.name = "variable") {
    arg <- list(...)
    for (i in 1 : length(arg)) {
        if (!inherits(arg[[i]], 'zoomly')) {
            stop('Some argument is not zoomly objects.')
        }
    }
    argnames <- names(arg)
    if (is.null(argnames)) {
        callobj <- sys.call()
        callList <- as.list(callobj)
        argnames <- as.character(callList[2 : (1 + length(arg))])
        names(arg) <- argnames
    }
    if (length(arg) == 1) {
        x <- arg[[1]]
        if (addname == TRUE) {
            cAttr <- data.frame(variable = argnames[1], month = attr(x, 'cattr'),
                               stringsAsFactors = FALSE)
            colnames(cAttr)[1] <- variable.name
            attr(x, 'cattr') <- cAttr
        }
        class(x) <- class(x)[-1]
        return(x)
    } else {
        arg <- zoomlyList(arg)
        z <- as.zoocat(arg)
        return(z)
    }
}


#'
#' @export
#' @rdname as.zoocat
as.zoocat.zoomlyList <- function (x, variable.name = 'variable', ...) {
    varname <- names(x)
    for (i in 1 : length(x)) {
        x[[i]] <- as.zoocat(x[[i]])
        colAttr <- cbind(name = varname[i], cattr(x[[i]]),
                         stringsAsFactors = FALSE)
        colnames(colAttr)[1] <- variable.name
        cattr(x[[i]]) <- colAttr
    }
    x <- unclass(x)
    zc <- do.call(merge, args = x)
    return(zc)
}


#' @export
#' @rdname as.zoocat
#' @param colattr a data frame the column attribute table for x.
#' @param index.name. the name of the index variable.
as.zoocat.zoo <- function (x, colattr = NULL, variable.name = 'variable', 
                           index.name. = 'index.name', ...) {
    stopifnot(length(dim(x))== 2)
    if (is.null(colattr)) {
        stopifnot(!is.null(colnames(x)))
        colattr <- data.frame(name = colnames(x))
        colnames(colattr) <- variable.name
    }
    attr(x, 'cattr') <- colattr
    attr(x, 'index.name') <- index.name.
    class(x) <- c('zoocat', class(x))
    return(x)
}


