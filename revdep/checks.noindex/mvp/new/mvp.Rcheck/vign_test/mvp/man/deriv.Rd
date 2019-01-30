\name{deriv}
\alias{deriv}
\alias{aderiv}
\alias{deriv.mvp}
\alias{deriv_mvp}
\alias{aderiv.mvp}
\alias{aderiv_mvp}
\title{Differentiation of mvp objects}
\description{
Differentiation of mvp objects}
\usage{
\method{deriv}{mvp}(expr, v, ...)
\method{aderiv}{mvp}(expr, ...)
}
\arguments{
  \item{expr}{mvp object}
  \item{v}{Character vector.  Elements denote variables to differentiate with respect to}
  \item{...}{Further arguments, ignored in \code{deriv()} but specifies
    the differentials in \code{aderiv()}}
}
\value{Returns its argument invisibly}
\author{Robin K. S. Hankin}

\details{

  Function \code{deriv(S,v)} returns \eqn{\frac{\partial^r S}{\partial
  v_1\partial v_2\ldots\partial v_r}}{d^rS/dv1...dvr}.

  Function \code{aderiv()} uses the ellipsis construction with the names
  of the argument being the variable to be differentiated with respect
  to.  Thus \code{aderiv(S,x=1,y=2)} returns \eqn{\frac{\partial^3
  S}{\partial x\partial y^2}}{d^3S/dxdy^2}.
}
\examples{
p <- rmvp(10,9,9,letters[1:4])
deriv(p,letters[1:3])
deriv(p,rev(letters[1:3]))  # should be the same

aderiv(p,a=1,b=2,c=1)

## verify the chain rule:
x <- rmvp(7,symbols=6)
v <- allvars(x)[1]
s <- as.mvp("1  +  y  -  y^2 zz  +  y^3 z^2")
LHS <- subsmvp(deriv(x,v)*deriv(s,"y"),v,s)   # dx/ds*ds/dy
RHS <- deriv(subsmvp(x,v,s),"y")              # dx/dy

LHS - RHS # should be zero

}
\keyword{symbolmath}
