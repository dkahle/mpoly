\name{drop}
\alias{drop}
\title{Drop empty variables}
\description{
Convert a mvp object which is a pure constant into a scalar whose value is
the coefficient of the empty term
}
\usage{
drop(S)
}
\arguments{
  \item{S}{An mvp object}
}
\author{Robin K. S. Hankin}
\seealso{\code{\link{subs}}}
\examples{

m1 <- as.mvp("1+bish +bash^2 + bosh^3")
m2 <- as.mvp("bish +bash^2 + bosh^3")

m1-m2         # an mvp object
drop(m1-m2)   # numeric
}