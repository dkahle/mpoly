\name{Ops.mvp}
\alias{Ops.mvp}
\alias{Ops}
\alias{mvp_negative}
\alias{mvp_times_mvp}
\alias{mvp_times_scalar}
\alias{mvp_plus_mvp}
\alias{mvp_plus_numeric}
\alias{mvp_plus_scalar}
\alias{mvp_power_scalar}
\alias{mvp_eq_mvp}
\title{Arithmetic Ops Group Methods for mvp objects}
\description{
  Allows arithmetic operators to be used for
  multivariate polynomials  such as addition, multiplication,
  integer powers, etc.
}
\usage{
\method{Ops}{mvp}(e1, e2)
mvp_negative(S)
mvp_times_mvp(S1,S2)
mvp_times_scalar(S,x)
mvp_plus_mvp(S1,S2)
mvp_plus_numeric(S,x)
mvp_eq_mvp(S1,S2)
}
\arguments{
  \item{e1,e2,S,S1,S2}{Objects of class \dQuote{\code{mvp}}}
  \item{x}{Scalar, length one numeric vector}
}
\details{

  The function \code{Ops.mvp()} passes unary and binary arithmetic
  operators \dQuote{\code{+}}, \dQuote{\code{-}}, \dQuote{\code{*}} and
  \dQuote{\code{^}} to the appropriate specialist function.

  The most interesting operator is \dQuote{\code{*}}, which is passed to
  \code{mvp_times_mvp()}.  I guess \dQuote{\code{+}} is quite
  interesting too.

}
\value{

  The high-level functions documented here return an object of
  \code{mvp}, the low-level functions documented at \code{lowlevel.Rd}
  return lists.   But don't use the low-level functions.

}
\author{Robin K. S. Hankin}
\seealso{\code{\link{lowlevel}}}
\examples{
p1 <- rmvp(3)
p2 <- rmvp(3)

p1*p2

p1+p2

p1^3


p1*(p1+p2) == p1^2+p1*p2  # should be TRUE

}
\keyword{symbolmath}
