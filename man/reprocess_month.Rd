% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reprocess_month.R
\name{reprocess_month}
\alias{reprocess_month}
\title{Reprocess month of \code{zoomly} objects}
\usage{
reprocess_month(x, mon.repro)
}
\arguments{
\item{x}{a \code{zoomly} object.}

\item{mon.repro}{new setting month vector. Can be integers larger than 12 or less than 1.}
}
\value{
a "\code{zoomly}" object.
}
\description{
Reprocess month of "\code{zoomly}" objects, make the objects contain the data 
corresponding to months of previous years and following years.
}
\details{
For example, if there is a data value corresponding to year of 1990 and month of Jan, 
the argument \code{month} for \code{reprocess_month} can be set to be 13, and we get 
data of "Jan.1" (means Jan of the following year, see \link{gmon}), and the year of 
that data value will be 1989.
}
\examples{
mat <- matrix(1:48, ncol = 12)
ctable <- data.frame(month = rep(1 : 12))
zm <- zoomly(mat, order.by = 1991 : 1994, colattr = ctable)
reprocess_month(zm, mon.repro = -11:2)
reprocess_month(zm, mon.repro = -24:3)
}

