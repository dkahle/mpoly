\name{spray}
\alias{spray}
\alias{spray_to_mvp}
\alias{mvp_to_spray}
\title{Spray functionality}
\description{Convert between spray and mvp form}
\usage{
spray_to_mvp(L, symbols = letters)
mvp_to_spray(S)
}

\arguments{
  \item{L}{mvp object}
  \item{symbols}{character vector of symbols}
  \item{S}{Spray object}
}
\author{Robin K. S. Hankin}
\examples{

mvp_to_spray(rmvp(5))
spray_to_mvp(spray::spray(diag(6),1:6))
}

\keyword{symbolmath}