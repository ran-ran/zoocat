
#' @export 
#' @rdname window
'window.zoomlyList' <- function (x, ...) {
    for (i in 1 : length(x)) {
        x[[i]] <- 'window'(x[[i]], ...)
    }
    return(x)
}


#' @export 
#' @rdname expand
'expand.zoomlyList' <- function (x, ...) {
    for (i in 1 : length(x)) {
        x[[i]] <- 'expand'(x[[i]], ...)
    }
    return(x)
}


#' @export 
#' @rdname normalize
'normalize.zoomlyList' <- function (x, ...) {
    for (i in 1 : length(x)) {
        x[[i]] <- 'normalize'(x[[i]], ...)
    }
    return(x)
}


#' @export 
'cummin.zoomlyList' <- function (x, ...) {
    for (i in 1 : length(x)) {
        x[[i]] <- 'cummin'(x[[i]], ...)
    }
    return(x)
}


#' @export 
'cumprod.zoomlyList' <- function (x, ...) {
    for (i in 1 : length(x)) {
        x[[i]] <- 'cumprod'(x[[i]], ...)
    }
    return(x)
}


#' @export 
'cumsum.zoomlyList' <- function (x, ...) {
    for (i in 1 : length(x)) {
        x[[i]] <- 'cumsum'(x[[i]], ...)
    }
    return(x)
}


#' @export 
'diff.zoomlyList' <- function (x, ...) {
    for (i in 1 : length(x)) {
        x[[i]] <- 'diff'(x[[i]], ...)
    }
    return(x)
}


#' @export 
'head.zoomlyList' <- function (x, ...) {
    for (i in 1 : length(x)) {
        x[[i]] <- 'head'(x[[i]], ...)
    }
    return(x)
}


#' @export 
'lag.zoomlyList' <- function (x, ...) {
    for (i in 1 : length(x)) {
        x[[i]] <- 'lag'(x[[i]], ...)
    }
    return(x)
}


#' @export 
'na.aggregate.zoomlyList' <- function (object, ...) {
    for (i in 1 : length(object)) {
        object[[i]] <- 'na.aggregate'(object[[i]], ...)
    }
    return(object)
}


#' @export 
'na.approx.zoomlyList' <- function (object, ...) {
    for (i in 1 : length(object)) {
        object[[i]] <- 'na.approx'(object[[i]], ...)
    }
    return(object)
}


#' @export 
'na.contiguous.zoomlyList' <- function (object, ...) {
    for (i in 1 : length(object)) {
        object[[i]] <- 'na.contiguous'(object[[i]], ...)
    }
    return(object)
}


#' @export 
'na.fill.zoomlyList' <- function (object, ...) {
    for (i in 1 : length(object)) {
        object[[i]] <- 'na.fill'(object[[i]], ...)
    }
    return(object)
}


#' @export 
'na.spline.zoomlyList' <- function (object, ...) {
    for (i in 1 : length(object)) {
        object[[i]] <- 'na.spline'(object[[i]], ...)
    }
    return(object)
}


#' @export 
'na.trim.zoomlyList' <- function (object, ...) {
    for (i in 1 : length(object)) {
        object[[i]] <- 'na.trim'(object[[i]], ...)
    }
    return(object)
}


#' @export 
'rollapply.zoomlyList' <- function (data, ...) {
    for (i in 1 : length(data)) {
        data[[i]] <- 'rollapply'(data[[i]], ...)
    }
    return(data)
}


#' @export 
'rollmax.zoomlyList' <- function (x, ...) {
    for (i in 1 : length(x)) {
        x[[i]] <- 'rollmax'(x[[i]], ...)
    }
    return(x)
}


#' @export 
'rollmean.zoomlyList' <- function (x, ...) {
    for (i in 1 : length(x)) {
        x[[i]] <- 'rollmean'(x[[i]], ...)
    }
    return(x)
}


#' @export 
'rollmedian.zoomlyList' <- function (x, ...) {
    for (i in 1 : length(x)) {
        x[[i]] <- 'rollmedian'(x[[i]], ...)
    }
    return(x)
}


#' @export 
'rollsum.zoomlyList' <- function (x, ...) {
    for (i in 1 : length(x)) {
        x[[i]] <- 'rollsum'(x[[i]], ...)
    }
    return(x)
}


#' @export 
'scale.zoomlyList' <- function (x, ...) {
    for (i in 1 : length(x)) {
        x[[i]] <- 'scale'(x[[i]], ...)
    }
    return(x)
}


#' @export 
'tail.zoomlyList' <- function (x, ...) {
    for (i in 1 : length(x)) {
        x[[i]] <- 'tail'(x[[i]], ...)
    }
    return(x)
}

