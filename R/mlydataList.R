#' mlydataList Class
#' 
#' mlydataList class.
#' 
#' 
#' @param x A list, each elements of which is a mlydata object. x must has
#' names for each elements.
#' @return A mlydataList object.
#' @examples
#' 
#' x <- matrix(1 : 20, nrow = 5)
#' x <- mlydata(x, year = 1991 : 1995, month = c(2, 3, 5, 6))
#' y <- x + 1
#' mdList <- mlydataList(list(x = x, y = y))
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




