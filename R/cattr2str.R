

cattr2str <- function (colattr) {
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

