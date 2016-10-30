#' A class for generalized month index
#' 
#' In the "\code{gmon}" class, a integer number is used to indicate the month.
#' The number from 1 to 12 is used to indicate Jan to Dec of the 
#' reference year. The number from -11 to 0 is used to indicate 
#' Jan to Dec of the previous year, and the number from 
#' 13 to 24 corresponds to the next year, and so on.
#' 
#' When print a "\code{gmon}" object, the suffix ".0" means the 
#' current year, ".1" means the next year and ".-1" means 
#' the previous year, and so on. 
#' For example, Feb of the next year is printed as "Feb.1".
#' The methods \code{scale_x_gmon} and \code{scale_y_gmon} are 
#' provided to show "\code{gmon}" objects properly based on \code{ggplot}.
#' 
#' @export
#' @examples 
#' mvec <- gmon(-20:25)
#' print(mvec)
#' x <- 1 : 46
#' names(x) <- mvec
#' print(x)
#' 
#' @param x a vector of integers.
#' @rdname gmon
gmon <- function (x) {
    ret <- round(x)
    class(ret) <- 'gmon'
    return(ret)
}


#' Coercion from and to \code{gmon}
#' @export
#' @param x an numeric vector.
#' @param ... unused.
as.gmon <- function (x, ...) {
    UseMethod('as.gmon')
}

#' @export
as.gmon.default <- function (x, ...) {
    gmon(as.numeric(x))
}


#' @export
print.gmon <- function (x, ...) { 
    print(as.character(x))
}

#' @export
as.character.gmon <- function (x, ...) {
    abb <- month.abb[true_month(x)]
    rela.yr <- rela_year(x)
    str <- paste(abb, '.', rela.yr, sep = '')
    return(str)
}

#' @export
as.data.frame.gmon <- function (x, row.names = NULL, optional = FALSE, ...)  {
    nrows <- length(x)
    nm <- paste(deparse(substitute(x), width.cutoff = 500), collapse = " ")
    if (is.null(row.names)) {
        if (nrows == 0) 
            row.names <- character(0)
        else if (length(row.names <- names(x)) == nrows && !any(duplicated(row.names))) {
        }
        else if (optional) 
            row.names <- character(nrows)
        else row.names <- seq_len(nrows)
    }
    names(x) <- NULL
    value <- list(x)
    if (!optional) 
        names(value) <- nm
    attr(value, "row.names") <- row.names
    class(value) <- "data.frame"
    value
}

#' @export
format.gmon <- function (x, ...) {
    return(format(as.character(x), ...))
}

#' @export
unique.gmon <- function (x, ...) {
    return(gmon(unique.default(x, ...)))
}



#' @export
"[.gmon" <- function (x, ..., drop = TRUE) 
{
    cl <- oldClass(x)
    class(x) <- NULL
    ret <- NextMethod("[")
    class(ret) <- cl
    return(ret)
}


#' @export
as.numeric.gmon <- function (x, ...) {
    unclass(x)
} 




#' Get the relative years for a \code{gmon} object
#' 
#' @export
#' @examples 
#' num.mon <- -12 : 3
#' gm <- gmon(num.mon)
#' ry <- rela_year(gm)
#' df <- data.frame(num.mon, ry, as.character(gm))
#' print(df)
#' 
#' @param x a \code{gmon} object.
rela_year <- function (x) {
    stopifnot(inherits(x, 'gmon'))
    class(x) <- NULL
    x <- x - 1
    ret <- (x - (x %% 12)) / 12
    return(ret)
}


#' Get the true month of a \code{gmon} object.
#' @export
#' @param x a \code{gmon} object.
#' @examples
#' num.mon <- -25 : 15
#' gm <- gmon(num.mon)
#' tm <- true_month(gm)
#' cbind(gm, tm)
true_month <- function (x) {
    stopifnot(inherits(x, 'gmon'))
    y <- x %% 12
    y[y == 0] <- 12
    return(y)
}





