% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ridge-reg.r
\name{ridge_reg}
\alias{ridge_reg}
\title{Fit a ridge regression model}
\usage{
ridge_reg(formula, lambda, data)
}
\arguments{
\item{formula}{a formula}

\item{lambda}{a penalty value}

\item{data}{a data.frame}
}
\value{
An ridge_reg object
}
\description{
This function passes parameters to the ridge_reg function.
}
\examples{
fit <- ridge_reg(Sepal.Length ~Sepal.Width+Petal.Length+Petal.Width , iris,lambda = 1)
summary(fit)
}
