#' Lissajous polynomials
#'
#' The Lissajous polynomials are the implicit (variety) descriptions of the
#' image of the parametric map x = cos(m t + p), y = sin(n t + q).
#'
#' @param m,n,p,q Trigonometric coefficients, see examples for description
#' @param digits The number of digits to round coefficients to, see
#'   [round.mpoly()]. This is useful for cleaning terms that are numerically
#'   nonzero, but should be.
#' @return a mpoly object
#' @seealso [chebyshev()], Merino, J. C (2003). Lissajous figures and Chebyshev
#'   polynomials. The College Mathematics Journal, 34(2), pp. 122-127.
#' @export
#' @examples
#'
#' lissajous(3, 2,  -pi/2, 0)
#' lissajous(4, 3,  -pi/2, 0)
#' 
lissajous <- function(m, n, p, q, digits = 3) {
  
  delta <- det(matrix(c(m, n, p, q), nrow = 2))
  Tnx <- chebyshev(n, indeterminate = "x")
  Tmy <- chebyshev(m, indeterminate = "y")
  is.even <- function(n) (n %% 2L) == 0L
  is.zero <- function(x, tol = 1e-5) abs(x) <= tol
  
  if (is.even(m)) {
    
    if (is.zero(sin(delta))) {
      p <- cos(delta)*Tnx - (-1)^(m/2)*Tmy
    } else {
      p <- Tnx^2 + Tmy^2 - 2*(-1)^(m/2)*Tnx*Tmy*cos(delta) - sin(delta)^2
    }
    
  } else { # m odd
    
    if (is.zero(cos(delta))) {
      p <- sin(delta)*Tnx - (-1)^((m-1)/2)*Tmy
    } else {
      p <- Tnx^2 + Tmy^2 - 2*(-1)^((m-1)/2)*Tnx*Tmy*sin(delta) - cos(delta)^2
    }  
    
  }
  
  round(p, digits)
  
}
