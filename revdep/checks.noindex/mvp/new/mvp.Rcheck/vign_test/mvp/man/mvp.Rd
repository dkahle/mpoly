\name{mvp}
\alias{mvp}
\alias{is.mvp}
\alias{as.mvp}
\alias{is_ok_mvp}
\title{Multivariate polynomials, mvp objects}
\description{Create, test for, an coerce to, mvp objects}
\usage{
mvp(vars, powers, coeffs)
is_ok_mvp(vars,powers,coeffs)
is.mvp(x)
as.mvp(x,...)
}
\arguments{
  \item{vars}{List of variables comprising each term of a mvp object}
  \item{powers}{List of powers corresponding to the variables of the
    \code{vars} argument}
  \item{coeffs}{Numeric vector corresponding to the coefficients to each
    element of the \code{var} and \code{powers} lists}
  \item{x}{Object possibly of class \code{mvp}}
  \item{...}{Further arguments, passed to the methods}
}
\details{
Function \code{mvp()} is the formal creation mechanism for \code{mvp}
objects.  However, it is not very user-friendly; it is better to use
\code{as.mvp()} in day-to-day use.

Function \code{is_ok_mvp()} checks for consistency of its arguments.

}
\author{Robin K. S. Hankin}
\examples{

mvp(list("x" , c("x","y"), "a",c("y","x")),list(1,1:2,3,c(-1,4)),1:4)

## Note how the terms appear in an arbitrary order, as do
## the symbols within a term.


kahle  <- mvp(
    vars   = split(cbind(letters,letters[c(26,1:25)]),rep(seq_len(26),each=2)),
    powers = rep(list(1:2),26),
    coeffs = 1:26
)

## again note arbitrary order of terms and symbols within a term




}