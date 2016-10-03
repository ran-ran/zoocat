

#' Tools for plottin \code{zoocat} objects with ggplot2
#' 
#' @examples 
#' library(ggplot2)
#' df <- data.frame(month = gmon(-5:5), value = 1:11)
#' ggplot(df, aes(month, value)) + 
#'     geom_line() + geom_point() +
#'     scale_x_gmon()
#'     
#' @export
#' @rdname ggplot.zoocat
gmon_trans <- function (n = 5) 
{
    breaks. <- function(x) as.yearmon(scales::pretty_breaks(n)(x))
    format. <- function(x) format(x)
    scales::trans_new("yearmon", transform = as.numeric, inverse = as.gmon, 
                      breaks = breaks., format = format.)
} 


#' @export
#' @rdname ggplot.zoocat
scale_x_gmon <- function (..., n = 5) {
    scale_x_continuous(..., trans = gmon_trans(n))
}


#' @export
#' @rdname ggplot.zoocat
scale_y_gmon <- function (..., n = 5) {
    scale_y_continuous(..., trans = gmon_trans(n))
}

