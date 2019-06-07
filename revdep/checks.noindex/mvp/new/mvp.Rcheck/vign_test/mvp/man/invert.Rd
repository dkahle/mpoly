\name{invert}
\alias{invert}
\title{Replace symbols with their reciprocals}
\description{
Given an mvp object, replace one or more symbols with their reciprocals
}
\usage{
invert(p, v)
}
\arguments{
  \item{p}{Object (coerced to) mvp form}
  \item{v}{Character vector of symbols to be replaced with their
    reciprocal; missing interpreted as replace all symbols}
}
\author{Robin K. S. Hankin}
\seealso{\code{\link{subs}}}
\examples{
invert("x")

invert(rmvp(10,7,7,letters[1:3]),"a")

}
