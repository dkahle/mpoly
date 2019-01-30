\name{kahle}
\alias{kahle}
\title{A sparse multivariate polynomial}
\description{A sparse multivariate polynomial inspired by Kahle (2013)}
\usage{
kahle(n = 26, r = 1, p = 1, coeffs = 1, symbols = letters)
}
\arguments{
  \item{n}{Number of different symbols to use}
  \item{r}{Number of symbols in a single term}
  \item{p}{Power of each symbol in each terms}
  \item{coeffs}{Coefficients of the terms}
  \item{symbols}{Alphabet of symbols}
}
\references{
David Kahle 2013.  \dQuote{\pkg{mpoly}: multivariate polynomials in R}.
\emph{R Journal}, volume 5/1.
}
\author{Robin K. S. Hankin}
\seealso{\code{\link{special}}}
\examples{
kahle()  # a+b+...+z
kahle(r=2,p=1:2)  # Kahle's original example

## example where mvp runs faster than spray (mvp does not need a 200x200 matrix):
k <- kahle(200,r=3,p=1:3,symbols=paste("x",sprintf("\%02d",1:200),sep=""))
system.time(ignore <- k^2)
#system.time(ignore <- mvp_to_spray(k)^2)   # needs spray package loaded
}
\keyword{symbolmath}
