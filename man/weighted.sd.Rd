% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/weighted.sd.R
\name{weighted.sd}
\alias{weighted.sd}
\title{Weighted Standard Deviation}
\usage{
weighted.sd(x, w = rep(1, length(x)), df = sum(w) - 1)
}
\arguments{
\item{x}{numeric vector of data}

\item{w}{numeric vector of weights}

\item{df}{degrees of freedom}
}
\value{
scalar value
}
\description{
Computes the standard deviation of values in x using weights.
}
\examples{

x <- c(1.5, 4.1, 3.7, 8.2)
w <- c(4, 5, 4, 1)
weighted.sd(x, w)


}
