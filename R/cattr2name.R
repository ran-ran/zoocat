

#' Transforming Data Frame of Column Attributes to Column Names Vector.
#' 
#' Transforming data frame of column attributes to column names vector, which
#' is useful for zoocat class.
#' 
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

