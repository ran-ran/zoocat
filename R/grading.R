#' Grading data by element numbers of each grade
#' 
#' Grading data by element numbers of each grade. The data must be a vector or 
#' a \code{zoo} object with the vector format.
#' 
#' 
#' @return If boundary.ret = TRUE, a list will be returned. The first element
#' is the grades of each element in x. The second is the boundaries between
#' grades. If boundary.ret = FALSE, only grades will be returned.
#' @examples
#' 
#' x <- zoo(1:10)
#' grading(x, alloc = c(2, 2, 6))
#' grading(x, alloc = c(2, 2, 6), grade.name = c('a', 'b', 'c'))
#' grading(x, alloc = c(2, 2, 6), grade.name = c(3, 3, 1))
#' grading(x, alloc = c(2, 2, 6), boundary.ret = TRUE)
#' 
#' x <- 1 : 10
#' grading(x, alloc = c(2, 2, 6), grade.name = c('a', 'b', 'c'))
#' 
#' @export
#' @name grading
#' @rdname grading
#' @param x a vector or a zoo object.
#' @param alloc the numbers of elements in each grade. sum(alloc) must ==
#' length(x).
#' @param boundary.ret logical. If TRUE, the boundaries between adjacent grades will be
#' returned.
#' @param grade.name character string. If NULL, the names of grades will be set to be 1, 2, ... ,
#' N. The 1st element of grade.name represents the grade of smallest value.
#' @param ... further arguments.
grading <- function (x, ...) {
    UseMethod('grading')
}


#' @export
#' @rdname grading
grading.default <- function (x, alloc, boundary.ret = FALSE, 
                             grade.name = NULL, ...) {
    x <- as.vector(x)
    stopifnot(sum(alloc) == length(x))
    stopifnot(all(alloc >= 1))
    if (is.null(grade.name)) {
        grade.name <- 1 : length(alloc)
    }
    stopifnot(length(grade.name) == length(alloc))

    xlen <- length(x)
    so <- sort(coredata(x), decreasing = F, index.return = TRUE)
    grade <- rep(NA, xlen)
    class(grade) <- class(grade.name)
    for (i in 1 : length(alloc)) {
        id1 <- cumsum(alloc)[i]
        id0 <- id1 - alloc[i] + 1
        grade[so$ix[id0 : id1]] <- grade.name[i]
    }

    if (boundary.ret == TRUE) {
        boundary <- rep(0, length(alloc) - 1)
        for (i in 1 : length(boundary)) {
            boundary[i] <- 0.5 * (max(coredata(x)[coredata(grade) == grade.name[i]])
                                  + min(coredata(x)[coredata(grade) == grade.name[i + 1]]))
        }
    }

    if (boundary.ret == TRUE) {
        return(list(grade = grade, boundary = boundary))
    } else {
        return(grade)
    }
}



#' @export
#' @rdname grading
grading.zoo <- function (x, ...) {
    stopifnot(is.zoo(x))
    if (!is.null(dim(x))) {
        stopifnot(ncol(x) == 1)
    }
    xcore <- coredata(x)
    ret <- grading(xcore, ...)
    if (is.list(ret)) {
        ret$grade <- zoo(ret$grade, order.by = index(x))
    } else {
        ret <- zoo(ret, order.by = index(x))
    }
    return(ret)
}
