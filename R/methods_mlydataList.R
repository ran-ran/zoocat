
#' @export 
#' @rdname window
'window.mlydataList' <- function (x, ...) {
    for (i in 1 : length(x)) {
        x[[i]] <- window(x[[i]], ...)
    }
    return(x)
}


#' @export 
#' @rdname expand
'cummax.mlydataList' <- function (x, ...) {
    for (i in 1 : length(x)) {
        x[[i]] <- cummax(x[[i]], ...)
    }
    return(x)
}


#' @export 
#' @rdname normalize
'normalize.mlydataList' <- function (x, ...) {
    for (i in 1 : length(x)) {
        x[[i]] <- normalize(x[[i]], ...)
    }
    return(x)
}


#' @export 
'cummin.mlydataList' <- function (x, ...) {
    for (i in 1 : length(x)) {
        x[[i]] <- cummin(x[[i]], ...)
    }
    return(x)
}


#' @export 
'cumprod.mlydataList' <- function (x, ...) {
    for (i in 1 : length(x)) {
        x[[i]] <- cumprod(x[[i]], ...)
    }
    return(x)
}


#' @export 
'cumsum.mlydataList' <- function (x, ...) {
    for (i in 1 : length(x)) {
        x[[i]] <- cumsum(x[[i]], ...)
    }
    return(x)
}


#' @export 
'diff.mlydataList' <- function (x, ...) {
    for (i in 1 : length(x)) {
        x[[i]] <- diff(x[[i]], ...)
    }
    return(x)
}


#' @export 
'head.mlydataList' <- function (x, ...) {
    for (i in 1 : length(x)) {
        x[[i]] <- head(x[[i]], ...)
    }
    return(x)
}


#' @export 
'lag.mlydataList' <- function (x, ...) {
    for (i in 1 : length(x)) {
        x[[i]] <- lag(x[[i]], ...)
    }
    return(x)
}


#' @export 
'na.aggregate.mlydataList' <- function (object, ...) {
    for (i in 1 : length(object)) {
        object[[i]] <- na.aggregate(object[[i]], ...)
    }
    return(object)
}


#' @export 
'na.approx.mlydataList' <- function (object, ...) {
    for (i in 1 : length(object)) {
        object[[i]] <- na.approx(object[[i]], ...)
    }
    return(object)
}


#' @export 
'na.contiguous.mlydataList' <- function (object, ...) {
    for (i in 1 : length(object)) {
        object[[i]] <- na.contiguous(object[[i]], ...)
    }
    return(object)
}


#' @export 
'na.fill.mlydataList' <- function (object, ...) {
    for (i in 1 : length(object)) {
        object[[i]] <- na.fill(object[[i]], ...)
    }
    return(object)
}


#' @export 
'na.spline.mlydataList' <- function (object, ...) {
    for (i in 1 : length(object)) {
        object[[i]] <- na.spline(object[[i]], ...)
    }
    return(object)
}


#' @export 
'na.trim.mlydataList' <- function (object, ...) {
    for (i in 1 : length(object)) {
        object[[i]] <- na.trim(object[[i]], ...)
    }
    return(object)
}


#' @export 
'rollapply.mlydataList' <- function (data, ...) {
    for (i in 1 : length(data)) {
        data[[i]] <- rollapply(data[[i]], ...)
    }
    return(data)
}


#' @export 
'rollmax.mlydataList' <- function (x, ...) {
    for (i in 1 : length(x)) {
        x[[i]] <- rollmax(x[[i]], ...)
    }
    return(x)
}


#' @export 
'rollmean.mlydataList' <- function (x, ...) {
    for (i in 1 : length(x)) {
        x[[i]] <- rollmean(x[[i]], ...)
    }
    return(x)
}


#' @export 
'rollmedian.mlydataList' <- function (x, ...) {
    for (i in 1 : length(x)) {
        x[[i]] <- rollmedian(x[[i]], ...)
    }
    return(x)
}


#' @export 
'rollsum.mlydataList' <- function (x, ...) {
    for (i in 1 : length(x)) {
        x[[i]] <- rollsum(x[[i]], ...)
    }
    return(x)
}


#' @export 
'scale.mlydataList' <- function (x, ...) {
    for (i in 1 : length(x)) {
        x[[i]] <- scale(x[[i]], ...)
    }
    return(x)
}


#' @export 
'tail.mlydataList' <- function (x, ...) {
    for (i in 1 : length(x)) {
        x[[i]] <- tail(x[[i]], ...)
    }
    return(x)
}

