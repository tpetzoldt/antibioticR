% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mxObj-coerce.R
\name{coerce}
\alias{coerce}
\alias{as.mxObj}
\alias{as.mxObj,list-method}
\alias{as.mxObj,vector-method}
\alias{as.list,mxObj-method}
\alias{as.vector,mxObj-method}
\title{Coercion Methods for 'mxObj' Univariate Mixture Objects}
\usage{
as.mxObj(from, ...)

\S4method{as.mxObj}{list}(from, ...)

\S4method{as.mxObj}{vector}(from, ...)

\S4method{as.list}{mxObj}(x, ...)

\S4method{as.vector}{mxObj}(x)
}
\arguments{
\item{from}{an object that is to be converted to another type}

\item{...}{reserved for future extension}

\item{x}{an object that is to be converted to another type}

\item{mxObj}{an object of class \code{'mxObj'}}
}
\description{
Conversion methods of
  univariate mixture distributions, composed of exponential, normal and gamma
  distributions.
}
