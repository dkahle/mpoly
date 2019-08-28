#' Chebyshev polynomials
#'
#' Chebyshev polynomials as computed by orthopolynom.
#'
#' @param degree degree of polynomial
#' @param kind \code{"t"} or \code{"u"} (Chebyshev polynomials of the first and
#'   second kinds), or \code{"c"} or \code{"s"}
#' @param indeterminate indeterminate
#' @param normalized provide normalized coefficients
#' @param k,n the k'th root of the n'th chebyshev polynomial
#' @return a mpoly object or mpolyList object
#' @author David Kahle calling code from the orthopolynom package
#' @seealso [orthopolynom::chebyshev.t.polynomials()],
#'   [orthopolynom::chebyshev.u.polynomials()],
#'   [orthopolynom::chebyshev.c.polynomials()],
#'   [orthopolynom::chebyshev.s.polynomials()],
#'   \url{http://en.wikipedia.org/wiki/Chebyshev_polynomials}
#' @name chebyshev
#' @examples
#'
#' chebyshev(0)
#' chebyshev(1)
#' chebyshev(2)
#' chebyshev(3)
#' chebyshev(4)
#' chebyshev(5)
#' chebyshev(6)
#' chebyshev(10)
#'
#' chebyshev(0:5)
#' chebyshev(0:5, normalized = TRUE)
#' chebyshev(0:5, kind = "u")
#' chebyshev(0:5, kind = "c")
#' chebyshev(0:5, kind = "s")
#' chebyshev(0:5, indeterminate = "t")
#'
#'
#'
#' # visualize the chebyshev polynomials
#'
#' library(ggplot2); theme_set(theme_classic())
#' library(tidyr)
#'
#' s <- seq(-1, 1, length.out = 201)
#' N <- 5 # number of chebyshev polynomials to plot
#' (cheb_polys <- chebyshev(0:N))
#'
#' # see ?bernstein for a better understanding of
#' # how the code below works
#'
#' df <- data.frame(s, as.function(cheb_polys)(s))
#' names(df) <- c("x", paste0("T_", 0:N))
#' mdf <- gather(df, degree, value, -x)
#' qplot(x, value, data = mdf, geom = "line", color = degree)
#'
#' 
#' 
#' # roots of chebyshev polynomials
#' N <- 5
#' cheb_roots <- chebyshev_roots(1:N, N)
#' cheb_fun <- as.function(chebyshev(N))
#' cheb_fun(cheb_roots)
#' 
#' 
#' 
#' # chebyshev polynomials are orthogonal in two ways:
#' T2 <- as.function(chebyshev(2))
#' T3 <- as.function(chebyshev(3))
#' T4 <- as.function(chebyshev(4))
#' 
#' w <- function(x) 1 / sqrt(1 - x^2)
#' integrate(function(x) T2(x) * T3(x) * w(x), lower = -1, upper = 1)
#' integrate(function(x) T2(x) * T4(x) * w(x), lower = -1, upper = 1)
#' integrate(function(x) T3(x) * T4(x) * w(x), lower = -1, upper = 1)
#' 
#' (cheb_roots <- chebyshev_roots(1:4, 4))
#' sum(T2(cheb_roots) * T3(cheb_roots) * w(cheb_roots))
#' sum(T2(cheb_roots) * T4(cheb_roots) * w(cheb_roots))
#' sum(T3(cheb_roots) * T4(cheb_roots) * w(cheb_roots))
#' 
#' sum(T2(cheb_roots) * T3(cheb_roots))
#' sum(T2(cheb_roots) * T4(cheb_roots))
#' sum(T3(cheb_roots) * T4(cheb_roots))
#' 
#'




#' @rdname chebyshev
#' @export
chebyshev <- function(degree, kind = "t", indeterminate = "x", normalized = FALSE){
  
  stopifnot(all(is.wholenumber(degree)))
  stopifnot(all(degree >= 0))
  
  
  ## deal with kind
  stopifnot(kind %in% c("t","u","c","s"))
  
  ## make coefs
  if(kind == "t") coefs <- chebyshev.t.polynomials(max(degree), normalized)
  if(kind == "u") coefs <- chebyshev.u.polynomials(max(degree), normalized)
  if(kind == "c") coefs <- chebyshev.c.polynomials(max(degree), normalized)
  if(kind == "s") coefs <- chebyshev.s.polynomials(max(degree), normalized)
  
  ## if only one degree is wanted, return that
  if(length(degree) == 1){
    coefs <- rev.default(coefs)[[1]]
    p <- as.mpoly.polynomial(coefs, indeterminate)
    class(p) <- c("chebyshev", "mpoly")
    attr(p, "chebyshev") <- list(
      "degree" = length(polynomial)-1, 
      "kind" = kind, 
      "indeterminate" = indeterminate,
      "normalized" = normalized
    )
    return(p)
  }
  
  ## if several are wanted, return them
  coefs <- coefs[degree+1]
  ps <- lapply(coefs, function(polynomial){
    p <- as.mpoly.polynomial(polynomial, indeterminate)
    class(p) <- c("chebyshev", "mpoly")
    attr(p, "chebyshev") <- list(
      "degree" = length(polynomial)-1, 
      "kind" = kind, 
      "indeterminate" = indeterminate,
      "normalized" = normalized
    )
    p
  })
  class(ps) <- "mpolyList"
  ps
  
}




#' @rdname chebyshev
#' @export
chebyshev_roots <- function(k, n) {
  stopifnot( is.wholenumber(k) )
  stopifnot( is.wholenumber(n) )
  stopifnot( round(k) %in% 1:n )
  cos((k - .5) * pi / n)
}



