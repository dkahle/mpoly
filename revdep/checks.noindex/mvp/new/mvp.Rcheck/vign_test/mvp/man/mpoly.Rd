\name{mpoly}
\alias{mpoly}
\alias{mpoly_to_mvp}
\alias{mvp_to_mpoly}
\alias{as.mpoly.mvp}
\title{Conversion to and from mpoly form}
\description{
  The \pkg{mpoly} package by David Kahle provides similar functionality
  to this package, and the functions documented here convert between
  mpoly and mvp objects.  The mvp package uses \code{mpoly::mp()} to
  convert character strings to mvp objects.
}
\usage{
mpoly_to_mvp(m)
\method{as.mpoly}{mvp}(x,...)
}
\arguments{
  \item{m}{object of class mvp}
  \item{x}{object of class mpoly}
  \item{...}{further arguments, currently ignored}
}
\author{Robin K. S. Hankin}
\seealso{\code{\link{spray}}}
\examples{

x <- rmvp(5)

x == mpoly_to_mvp(mpoly::as.mpoly(x))        # should be TRUE
}
\keyword{symbolmath}
