#' A class for generalized month index
#' 
#' @export
#' @examples 
#' mvec <- gmon(-20:25)
#' print(mvec)
#' x <- 1 : 46
#' names(x) <- mvec
#' print(x)
#' 
gmon <- function (x) {
    ret <- x
    class(ret) <- 'gmon'
    return(ret)
}


print.gmon <- function (x, ...) { 
    print(as.character(x))
}

as.character.gmon <- function (x, ...) {
    abb <- month.abb[true_month(x)]
    rela.yr <- rela_year(x)
    str <- paste(abb, '.', rela.yr, sep = '')
    return(str)
}

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

#' Get the relative years for a \code{gmon} object
#' 
#' @export
#' @examples 
#' num.mon <- -12 : 3
#' gm <- gmon(num.mon)
#' ry <- rela_year(gm)
#' df <- data.frame(num.mon, ry, as.character(gm))
#' print(df)
rela_year <- function (x) {
    stopifnot(inherits(x, 'gmon'))
    class(x) <- NULL
    x <- x - 1
    ret <- (x - (x %% 12)) / 12
    return(ret)
}


#' Get the true month of a \code{gmon} object.
#' @export
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





