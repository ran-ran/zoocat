

#' Combine Data Frames by Rows
#' 
#' Combine data frames by rows. The data frames can have different number of
#' columns or different column names, which is different with \code{rbind}
#' 
#' The columns of data frame should not be class "factor".
#' 
#' @usage rowbind(dfList)
#' @param dfList A list of data frames.
#' @return A data frame.
#' @examples
#' 
#' colAttr1 <- data.frame(month = c(2, 3, 5, 6), name = 'sst', stringsAsFactors = F)
#' colAttr2 <- data.frame(month = c(4, 6, 7, 9), level = 500)
#' colAttr3 <- data.frame(id = 10 : 14, name = 'slp', stringsAsFactors = F)
#' rowbind(list(colAttr1, colAttr2, colAttr3))
#' 
#' @export rowbind
rowbind <- function (dfList) {
    cname <- c()
    colclass <- c()
    numRow <- 0
    for (i in 1 : length(dfList)) {
        cname <- c(cname, colnames(dfList[[i]]))
        for (j in 1 : ncol(dfList[[i]])) {
            colclass <- c(colclass, class(dfList[[i]][, j]))
        }
        numRow <- numRow + nrow(dfList[[i]])
    }
    if (any(colclass == 'factor')) {
        stop('The class of columns of the data frames in dfList should not be factor.')
    }
    cnameUnique <- unique(cname)
    colClassUnique <- rep('', length(cnameUnique))
    for (i in 1 : length(cnameUnique)) {
        colClassUnique[i] <- colclass[which(cname == cnameUnique[i])[1]]
    }
    dframeTotal <- as.data.frame(matrix(0, nrow = numRow, ncol = length(cnameUnique)))
    colnames(dframeTotal) <- cnameUnique
    for (i in 1 : length(cnameUnique)) {
        if (colClassUnique[i] == 'numeric' | colClassUnique[i] == 'integer') {
            dframeTotal[, i] <- NA
        } else {
            if (colClassUnique[i] == 'character') {
                dframeTotal[, i] <- 'NA'
            } else {
                stop('Invalid class for rowbind, especially factor.')
            }
        }
        class(dframeTotal[, i]) < colClassUnique[i]
    }
    idrow1 <- 1
    for (i in 1 : length(dfList)) {
        dframeNow <- dfList[[i]]
        idrow2 <- idrow1 + nrow(dframeNow) - 1
        cnameNow <- colnames(dframeNow)
        for (j in 1 : ncol(dframeNow)) {
            dframeTotal[idrow1 : idrow2, cnameNow[j]] <- dframeNow[, j]
        }
        idrow1 <- idrow2 + 1
    }
    return(dframeTotal)
}



