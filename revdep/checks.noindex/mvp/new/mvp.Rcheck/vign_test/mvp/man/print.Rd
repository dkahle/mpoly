\name{print}
\alias{print.mvp}
\alias{print_mvp}
\title{
Print methods for mvp objects
}
\description{
Print methods for mvp objects: to print, an mvp object is coerced to
\code{mpoly} form and the \code{mpoly} print method used.

}
\usage{
\method{print}{mvp}(x, ...)
}
\arguments{
  \item{x}{mvp object, coerced to \code{mpoly} form}
  \item{...}{Further arguments}
}
\value{Returns its argument invisibly}
\author{Robin K. S. Hankin}
\examples{

a <- rmvp(4)
a
print(a)
print(a,stars=TRUE)
print(a,varorder=rev(letters))
}
\keyword{symbolmath}
