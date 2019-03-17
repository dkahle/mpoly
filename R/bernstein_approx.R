#' Bernstein polynomial approximation
#'
#' Bernstein polynomial approximation
#'
#' @param f the function to approximate
#' @param n Bernstein polynomial degree
#' @param lower lower bound for approximation
#' @param upper upper bound for approximation
#' @param indeterminate indeterminate
#' @param ... ...
#' @return a mpoly object
#' @author David Kahle
#' @name bernstein-approx
#' @examples
#'
#'
#'
#'
#'
#' \dontrun{  # visualize the bernstein polynomials
#'
#' library(ggplot2); theme_set(theme_bw())
#' library(reshape2)
#'
#'
#'
#'
#' f <- function(x) sin(2*pi*x)
#' p <- bernstein_approx(f, 20)
#' round(p, 3)
#'
#' x <- seq(0, 1, length.out = 101)
#' df <- data.frame(
#'   x = rep(x, 2),
#'   y = c(f(x), as.function(p)(x)),
#'   which = rep(c("actual", "approx"), each = 101)
#' )
#' qplot(x, y, data = df, geom = "line", color = which)
#'
#'
#'
#'
#'
#'
#' p <- bernstein_approx(sin, 20, pi/2, 1.5*pi)
#' round(p, 4)
#'
#' x <- seq(0, 2*pi, length.out = 101)
#' df <- data.frame(
#'   x = rep(x, 2),
#'   y = c(sin(x), as.function(p)(x)),
#'   which = rep(c("actual", "approx"), each = 101)
#' )
#' qplot(x, y, data = df, geom = "line", color = which)
#'
#'
#'
#'
#'
#'
#'
#'
#' p <- bernstein_approx(dnorm, 15, -1.25, 1.25)
#' round(p, 4)
#'
#' x <- seq(-3, 3, length.out = 101)
#' df <- data.frame(
#'   x = rep(x, 2),
#'   y = c(dnorm(x), as.function(p)(x)),
#'   which = rep(c("actual", "approx"), each = 101)
#' )
#' qplot(x, y, data = df, geom = "line", color = which)
#'
#'
#'
#'
#'
#'
#' }
#' 


#' @rdname bernstein-approx
#' @export
bernstein_approx <- function(f, n, lower = 0, upper = 1, indeterminate = "x"){  
  
  ## compute support and determine weights
  s <- (0:n)/n
  fscaled <- function(.) f( (upper-lower)*. + lower )
  weights <- as.list(fscaled(s))
  
  ## convert weights to mpolyList
  weights <- lapply(weights, function(x) mpoly(list(c(coef = x))))
  class(weights) <- "mpolyList"
  
  ## multiply weights by basis
  approxPoly <- Reduce(`+`, weights * bernstein(0:n, n, "temp"))  
  
  ## compute plugin and plug in
  pluginPoly <- (upper-lower)^-1 * (mp(indeterminate) + -1*lower)
  plug(approxPoly, "temp", pluginPoly)
  
}


#' @rdname bernstein-approx
#' @export
bernsteinApprox <- function(...) {
  .Deprecated("bernstein_approx")
  bernstein_approx(...)
}