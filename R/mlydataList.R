#' mlydataList Class
#' 
#' mlydataList class.
#' 
#' 
#' @usage mlydataList(x)
#' @param x A list, each elements of which is a mlydata object. x must has
#' names for each elements.
#' @return A mlydataList object.
#' @examples
#' 
#' x <- matrix(1 : 20, nrow = 5)
#' md1 <- mlydata(x, year = 1991 : 1995, month = c(2, 3, 5, 6))
#' md2 <- md1 + 1
#' mdList <- mlydataList(list(md1 = md1, md2 = md2))
#' as.zoocat(mdList)
#' 
#' @export
mlydataList <- function(x) {
    if(!is.list(x)) 
        stop('x must be a list.')
    for(i in 1 : length(x)) {
        if(all(class(x[[i]]) != 'mlydata')) {
            stop('The elements of x must be mlydata objects.')
        }
    }
    if(is.null(names(x)))
        stop('names(x) must be specified.')
    class(x) <- 'mlydataList'
    return(x)
}




