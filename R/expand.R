
#' Expand \code{mlydata} objects
#' 
#' @name expand
#' @rdname expand
#' @export
expand <- function (x, ...) {
    UseMethod('expand')
}


#' @rdname expand
#' 
#' @examples
#' 
#' x <- matrix(1 : 48, nrow = 4, byrow = TRUE)
#' md <- mlydata(x, year = 1991 : 1994)
#' expand(md, left = -2)
#' expand(md, right = 15)
#' expand(md, right = 15, naTrim = FALSE)
#' expand(md, left = -13, right = 14)
#' expand(md, left = -25, right = 14, naTrim = FALSE)
#' 
#' @param x the \code{mlydata} object.
#' @param left the month of the left limit.
#' @param right the month of the right limit.
#' @param naTrim logical. If TRUE, na.trim will be used to trim the
#' result.
#' @param ... further arguments input to \code{na.trim}.
#' @return a \code{mlydata} or \code{mlydataList} object.
#' @export
expand.mlydata <- function (x, left = 1, right = 12, naTrim = TRUE,
                            ...) {
    if (ncol(x) != 12 || !all(attr(x, 'month') == 1 : 12)) {
        stop('the month of x must be 1 : 12.')
    }
    if (right < 12) {
        warning('neglect invalid argument right.')
    }
    if (left > 1) {
        warning('neglect invalid argument left.')
    }
    ret <- x
    
    if (left < 1) {
        nlagLeft <- floor(-left / 12) + 1
        vecLagLeft <- seq(from = -nlagLeft, to = -1, by = 1)
        ncolLeft <- (-left + 1) %% 12
        leftAdd <- lag(x[, (12 - ncolLeft + 1) : 12], k = -nlagLeft)
        if (nlagLeft > 1) {
            for (i in 2 : nlagLeft) {
                leftAdd <- merge(leftAdd, lag(x, k = vecLagLeft[i]))
            }
        }
        ret <- merge(leftAdd, ret)
    }
    if (right > 12) {
        nlagRight <- floor((right - 1) / 12)
        vecLagRight <- seq(from = nlagRight, to = 1, by = -1)
        ncolRight <- right %% 12
        rightAdd <- lag(x[, 1 : ncolRight], k = nlagRight)
        if (nlagRight > 1) {
            for (i in 2 : nlagRight) {
                leftAdd <- merge(lag(x, k = vecLagLeft[i]), rightAdd)
            }
        }
        ret <- merge(ret, rightAdd)
    }
    if (naTrim == TRUE) {
        ret <- na.trim(ret, ...)
    }
    return(ret)
}




