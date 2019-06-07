\name{zero}
\alias{zero}
\alias{is.zero}
\title{The zero polynomial}
\description{
Test for a polynomial being zero
}
\usage{
is.zero(x)
}
\arguments{
  \item{x}{Object of class mvp}
}
\details{

  Function \code{is.zero()} returns \code{TRUE} if \code{x} is indeed
  the zero polynomial.  It is defined as \code{length(vars(x))==0} for
  reasons of efficiency, but conceptually it returns
  \code{x==constant(0)}.

  (Use \code{constant(0)} to create the zero polynomial).

}
\author{Robin K. S. Hankin}
\note{

I would have expected the zero polynomial to be problematic (cf the
\pkg{freegroup} and \pkg{permutations} packages, where similar issues
require extensive special case treatment).  But it seems to work fine,
which is a testament to the robust coding in the STL.

A general mvp object is something like

\code{\{"x" -> 3, "y" -> 5\} -> 6, \{"x" -> 1, "z" -> 8\} -> -7\}}



which would be \eqn{6x^3y^5-7xz^8}{6x^3*y^5-7x*z^8}.

The zero polynomial is just \code{\{\}}.  Neat, eh?
  
}

\seealso{\code{\link{constant}}}
\examples{

constant(0)

t1 <- as.mvp("x+y")
t2 <- as.mvp("x-y")

stopifnot(is.zero(t1*t2-as.mvp("x^2-y^2")))

}

\keyword{symbolmath}
