\name{subs}
\alias{subs}
\alias{subsy}
\alias{substitute}
\alias{subsmvp}
\alias{subs_mvp}
\alias{mvp_subs_mvp}
\title{Substitution}
\description{Substitute symbols in an mvp object for numbers or other
  multivariate polynomials
}
\usage{
subs(S, ..., drop = TRUE)
subsy(S, ..., drop = TRUE)
subsmvp(S,v,X)
}
\arguments{
  \item{S,X}{Multivariate polynomials}
  \item{\dots}{named arguments corresponding to variables to substitute}
  \item{drop}{Boolean with default \code{TRUE} meaning to return a
    scalar (the constant) in place of a constant mvp object}
  \item{v}{A string corresponding to the variable to substitute}
}
\value{
  Return a multivariate polynomial, object of class mvp
}
\details{
Function \code{subs()} uses a natural R idiom to substitute scalar
values for symbols.

Observe that this type of substitution is sensitive to order:

\preformatted{
> p <- as.mvp("a b^2")
> subs(p,a="b",b="x")
mvp object algebraically equal to
x^3
> subs(p,b="x",a="b")
mvp object algebraically equal to
b x^2
}

Functions \code{subsy()} and \code{subsmpv()} are lower-level functions,
not really intended for the end-user.  Function \code{subsy()}
substitutes variables for numeric values (order matters if a variable is
substitued more than once).  Function \code{subsmpv()} takes a mvp
object and substitutes another mvp object for a specific symbol.  
}
\author{Robin K. S. Hankin}
\seealso{\code{\link{drop}}}
\examples{
p <- rmvp(6,2,2,letters[1:3])
p
subs(p,a=1)
subs(p,a=1,b=2)

subs(p,a="1+b x^3",b="1-y")
subs(p,a=1,b=2,c=3,drop=FALSE)

do.call(subs,c(list(as.mvp("z")),rep(c(z="C+z^2"),5)))

}
\keyword{symbolmath}
