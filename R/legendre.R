#' Legendre polynomials
#'
#' Legendre polynomials as computed by orthopolynom.
#'
#' @param degree degree of polynomial
#' @param indeterminate indeterminate
#' @param normalized provide normalized coefficients
#' @return a mpoly object or mpolyList object
#' @author David Kahle calling code from the orthopolynom package
#' @seealso [orthopolynom::legendre.polynomials()],
#'   \url{http://en.wikipedia.org/wiki/Legendre_polynomials}
#' @export
#' @examples
#'
#' legendre(0)
#' legendre(1)
#' legendre(2)
#' legendre(3)
#' legendre(4)
#' legendre(5)
#' legendre(6)
#'
#' legendre(2)
#' legendre(2, normalized = TRUE)
#'
#' legendre(0:5)
#' legendre(0:5, normalized = TRUE)
#' legendre(0:5, indeterminate = "t")
#'
#'
#'
#' # visualize the legendre polynomials
#'
#' library(ggplot2); theme_set(theme_classic())
#' library(tidyr)
#'
#' s <- seq(-1, 1, length.out = 201)
#' N <- 5 # number of legendre polynomials to plot
#' (legPolys <- legendre(0:N))
#'
#' # see ?bernstein for a better understanding of
#' # how the code below works
#'
#' df <- data.frame(s, as.function(legPolys)(s))
#' names(df) <- c("x", paste0("P_", 0:N))
#' mdf <- gather(df, degree, value, -x)
#' qplot(x, value, data = mdf, geom = "line", color = degree)
#'
#'
#' # legendre polynomials and the QR decomposition
#' n <- 201
#' x <- seq(-1, 1, length.out = n)
#' mat <- cbind(1, x, x^2, x^3, x^4, x^5)
#' Q <- qr.Q(qr(mat))
#' df <- as.data.frame(cbind(x, Q))
#' names(df) <- c("x", 0:5)
#' mdf <- gather(df, degree, value, -x)
#' qplot(x, value, data = mdf, geom = "line", color = degree)
#' 
#' Q <- apply(Q, 2, function(x) x / x[n])
#' df <- as.data.frame(cbind(x, Q))
#' names(df) <- c("x", paste0("P_", 0:5))
#' mdf <- gather(df, degree, value, -x)
#' qplot(x, value, data = mdf, geom = "line", color = degree)
#' 
#' 
#' # chebyshev polynomials are orthogonal in two ways:
#' P2 <- as.function(legendre(2))
#' P3 <- as.function(legendre(3))
#' P4 <- as.function(legendre(4))
#' 
#' integrate(function(x) P2(x) * P3(x), lower = -1, upper = 1)
#' integrate(function(x) P2(x) * P4(x), lower = -1, upper = 1)
#' integrate(function(x) P3(x) * P4(x), lower = -1, upper = 1)
#' 
#' n <- 10000L
#' u <- runif(n, -1, 1)
#' 2 * mean(P2(u) * P3(u))
#' 2 * mean(P2(u) * P4(u))
#' 2 * mean(P3(u) * P4(u))
#' 
#' (2/n) * sum(P2(u) * P3(u))
#' (2/n) * sum(P2(u) * P4(u))
#' (2/n) * sum(P3(u) * P4(u))
#' 
legendre <- function(degree, indeterminate = "x", normalized = FALSE){
  
  stopifnot(all(is.wholenumber(degree)))
  stopifnot(all(degree >= 0))
  
  ## make coefs
  coefs <- legendre.polynomials(max(degree), normalized = normalized)
  
  ## if only one degree is wanted, return that
  if(length(degree) == 1){
    coefs <- rev.default(coefs)[[1]]
    p <- as.mpoly.polynomial(coefs, indeterminate)
    class(p) <- c("legendre", "mpoly")
    attr(p, "legendre") <- list(degree = length(coefs)-1, indeterminate = indeterminate, normalized = normalized)
    return(p)
  }
  
  ## if several are wanted, return them
  coefs <- coefs[degree+1]
  ps <- lapply(coefs, function(polynomial){
    p <- as.mpoly.polynomial(polynomial, indeterminate)
    class(p) <- c("legendre", "mpoly")
    attr(p, "legendre") <- list(degree = length(polynomial)-1, indeterminate = indeterminate, normalized = normalized)
    p
  })
  class(ps) <- "mpolyList"
  ps
  
}



