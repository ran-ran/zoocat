

#' Transforming Data Frame of Column Attributes to Column Names Vector.
#' 
#' Transforming data frame of column attributes to column names vector, which
#' is useful for zoocat class.
#' 
#' If a column' name of colattr is "lat", style of "N/S" will be used. If a
#' column's name is "month", negative value will be transformed as "neg" +
#' abs(month).
#' 
#' @usage cattr2name(colattr)
#' @param colattr A data frame of column attributes in zoocat object.
#' @return A character vector.
#' @examples
#' 
#' colAttr <- data.frame(month = c(-2, -3, 5, 6), lat = c(-20, -10, 0, 10))
#' cattr2name(colAttr)
#' 
#' @export
cattr2name <- function (colattr) {
    attrName <- colnames(colattr)
    nAttr <- length(attrName)
    if ('lat' %in% attrName) {
        latVec <- colattr[, 'lat']
        latVecChar <- latVec
        latVecChar[latVec > 0] <- paste(latVec[latVec > 0], 'N', sep = '')
        latVecChar[latVec < 0] <- paste(-1 * latVec[latVec < 0], 'S', sep = '')
        colattr[, 'lat'] <- latVecChar
    }
    if ('month' %in% attrName) {
        monthVec <- colattr[, 'month']
        monthVec[monthVec < 0] <- paste('neg', -1 * monthVec[monthVec < 0], sep = '')
        colattr[, 'month'] <- monthVec
    }
    cNameVec <- NULL
    for (i in 1 : nAttr) {
        if (i < nAttr) {
            cNameVec <- paste(cNameVec, colattr[, i], '_', sep = '')
        } else {
            cNameVec <- paste(cNameVec, colattr[, i], sep = '')
        }
    }
    return(cNameVec)
}

