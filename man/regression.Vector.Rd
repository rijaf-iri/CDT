% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cdtRegression_functions.R
\name{regression.Vector}
\alias{regression.Vector}
\title{Vectorization, regression}
\usage{
regression.Vector(X, Y, min.len)
}
\arguments{
\item{X}{A vector of length N.}

\item{Y}{A numeric NxK matrix.}

\item{min.len}{An integer specify the minimum length of non missing data.}
}
\value{
Returns a numeric 10xK matrix.
}
\description{
Regression with a matrix and a vector performed by column \code{lm(Y~X)}.
}
\examples{
library(CDT)

set.seed(1)
X <- rnorm(20)
Y <- matrix(rnorm(200), nrow = 20, ncol = 10)
res <- regression.Vector(X, Y, 10)

}
