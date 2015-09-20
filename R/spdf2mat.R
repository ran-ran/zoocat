#' Translate Data Frame of Spatial Data to Matrix
#' 
#' Translate a data frame of spatial data to a matrix. The data frame must contain 
#' columns of latitude, longitude and values.
#' 
#' @param x A data frame.
#' @param lat.var Name of column which store latitude.
#' @param lon.var Name of column which store longitude.
#' @param value.var Name of column which store values.
#' @return A list contain matrix and bbox.
#' @export
#' @examples
#' 
#' x <- data.frame(lat = rep(1:10, each = 10),
#'                 lon = rep(1:10, 10),
#'                 value = 1:100)
#' mat <- spdf2mat(x)
#' 
#' 
spdf2mat <- function (x, lat.var = 'lat', lon.var = 'lon', value.var = 'value') {
    stopifnot(is.data.frame(x))
    stopifnot(all(c(lat.var, lon.var, value.var) %in% colnames(x)))
    x <- x[, c(lat.var, lon.var, value.var)]
    colnames(x) <- c('lat', 'lon', 'value')
    sp::coordinates(x) <- ~lon+lat
    sp::gridded(x) <- TRUE
    sp::fullgrid(x) <- TRUE
    mat <- as.matrix(x)
    mat <- t(mat)
    mat <- mat[nrow(mat) : 1, ]
    ret <- list(mat = mat, bbox = sp::bbox(x))
    return(ret)
}
