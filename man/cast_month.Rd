% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/yearmon_style.R
\name{cast_month}
\alias{cast_month}
\title{Cast month of a \code{zoomly} object}
\usage{
cast_month(x)
}
\arguments{
\item{x}{a zoocat object.}
}
\description{
Cast month of a \code{zoomly} object
}
\examples{
x <- matrix(1 : 20, nrow = 5)
colAttr <- data.frame(month = c(2, 3, 5, 6), name = c(rep('xxx', 3), 'yyy'))
zc <- zoocat(x, order.by = 1991 : 1995, colattr = colAttr)
zmelt <- melt_month(as.zoomly(zc))
cast_month(zmelt)

}

