\name{accessor}
\alias{accessors}
\alias{vars}
\alias{powers}
\alias{coeffs}
\alias{coeffs<-}
\alias{coeffs<-.mvp}
\alias{coefficients}
\title{Accessor methods for mvp objects}
\description{Accessor methods for mvp objects}
\usage{
vars(x)
powers(x)
coeffs(x)
coeffs(x) <- value
}
\arguments{
  \item{x}{Object of class mvp}
  \item{value}{Numeric vector of length 1}
}
\details{
Access the different parts of an mvp object.  The constant term is
technically a coefficient but is documented under \code{constant.Rd}. 
}
\author{Robin K. S. Hankin}
\note{

The terms of an \code{mvp} object are not held in any particular order;
the order of the terms is not defined.  This is because the \code{map}
class of the \code{STL} does not specify an order for the key-value
pairs (which may be implementation dependent).  The situation is similar
to the \code{hyper2} package which uses a similar scheme.

So the output of \code{coeffs(x)} is defined only up to an unknown
rearrangement.  If all the coefficients are the same, this does not
matter.  The same considerations apply to the output of \code{vars()}
and the output of \code{powers()} (which return lists whose elements are
in an undefined order).

However, even though the order of these three objects is undefined
individually, their ordering is jointly consistent in the sense that the
first element of \code{coeffs(x)} corresponds to the first element of
\code{vars(x)} and the first element of \code{powers(x)}.  The identity
of this element is not defined---but whatever it is, the first element
of all three accessor methods refers to it.

The vignette discusses this.

}
\seealso{\code{\link{constant}}}
\examples{
a <- rmvp(5)
vars(a)
powers(a)
coeffs(a)

coeffs(a) <- 1  # A simpler object
coeffs(a) <- 0  # The zero polynomial

}