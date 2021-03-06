% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/JSp2.R
\name{JS.p2}
\alias{JS.p2}
\title{Format P value}
\usage{
JS.p2(value)
}
\arguments{
\item{value}{numeric vector of p-value (possibly with NAs )}
}
\value{
A numeric vector of corrected p-value
}
\description{
JS.p2 is intended for formatting p-value , the difference with JS.p is will output all numeric data
}
\examples{
JS.p(0.060)
JS.p(0.0050)
JS.p(0.00001)
}
