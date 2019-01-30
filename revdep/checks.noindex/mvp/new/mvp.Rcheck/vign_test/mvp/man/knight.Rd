\name{knight}
\alias{knight_mvp}
\alias{knight}
\title{Chess knight}
\description{
Generating function for a chess knight on an infinite $d$-dimensional chessboard
}
\usage{
knight(d, can_stay_still = FALSE)
}
\arguments{
  \item{d}{Dimension of the board}
  \item{can_stay_still}{Boolean, with default \code{FALSE} meaning that
    the knight is obliged to move and \code{FALSE} meaning that it has
    the option of remaining on its square}
}
\author{Robin K. S. Hankin}
\note{The function is a slight modification of \code{spray::knight()}.}
\examples{

knight(2)      # regular chess knight on a regular chess board
knight(2,TRUE) # regular chess knight that can stay still

# Q: how many ways are there for a 4D knight to return to its starting
# square after four moves?

# A:
constant(knight(4)^4)

# Q ...and how many ways in four moves or fewer?

# A1:
constant(knight(4,TRUE)^4)

# A2:
constant((1+knight(4))^4)

}
\keyword{symbolmath}
