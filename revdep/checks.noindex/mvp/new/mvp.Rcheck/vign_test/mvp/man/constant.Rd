\name{constant}
\alias{constant}
\alias{constant}
\alias{constant<-}
\alias{constant.mvp}
\alias{constant<-.mvp}
\alias{constant.numeric}
\title{The constant term}
\description{Get and set the constant term of a mvp object}
\usage{
\method{constant}{mvp}(x)
\method{constant}{mvp}(x) <- value
\method{constant}{numeric}(x)
}
\arguments{
  \item{x}{Object of class mvp}
  \item{value}{Scalar value for the constant}
}
\details{

  The constant term in a polynomial is the coefficient of the empty term.
  In an mvp object, the map \code{\{\} -> c}, implies that \code{c} is
  the constant.

  If \code{x} is an mvp object, \code{constant(x)} returns the value of
  the constant in the multivariate polynomial; if \code{x} is numeric,
  it returns a constant multivariate polynomial with value \code{x}.
  
}
\author{Robin K. S. Hankin}
\examples{
a <- rmvp(5)+4
constant(a)
constant(a) <- 33
a

constant(0)  # the zero mvp
}
