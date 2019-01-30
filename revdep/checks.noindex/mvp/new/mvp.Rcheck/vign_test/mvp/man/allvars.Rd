\name{allvars}
\alias{allvars}
\title{All variables in a multivariate polynomial}
\description{
Returns a character vector containing all the variables present in a mvp object
}
\usage{
allvars(x)
}
\arguments{
  \item{x}{object of class mvp}
}
\author{Robin K. S. Hankin}
\note{The character vector returned is not in any particular order}
\examples{
p <- rmvp(5)
allvars(p)
}
\keyword{symbolmath}

