\name{special}
\alias{special}
\alias{product}
\alias{homog}
\alias{linear}
\alias{xyz}
\alias{numeric_to_mvp}
\title{Various functions to create simple multivariate polynomials}
\description{
Various functions to create simple mvp objects such as single-term,
homogenous, and constant multivariate polynomials.
}
\usage{
product(v,symbols=letters)
homog(d,power=1,symbols=letters)
linear(x,power=1,symbols=letters)
xyz(n,symbols=letters)
numeric_to_mvp(x)
}
\arguments{
  \item{d,n}{An integer; generally, the dimension or arity of the resulting mvp}
  \item{v,power}{Integer vector of powers}
  \item{x}{Numeric vector of coefficients}
  \item{symbols}{Character vector for the symbols}
}
\value{All functions documented here return a mvp object}
\author{Robin K. S. Hankin}
\note{

The functions here are related to their equivalents in the multipol and
spray packages, but are not exactly the same.

Function \code{constant()} is documented at \code{constant.Rd}, but is listed
below for convenience.
}
\seealso{\code{\link{constant}}, \code{\link{zero}}}
\examples{
product(1:3)        #      a * b^2 * c^3
homog(3)            #      a + b + c
homog(3,2)          #      a^2  + a b + a c + b^2 + b c + c^2
linear(1:3)         #      1*a + 2*b + 3*c
constant(5)         #      5
xyz(5)              #      a*b*c*d*e
}
\keyword{symbolmath}
