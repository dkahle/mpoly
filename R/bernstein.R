#' Bernstein polynomials
#'
#' Bernstein polynomials
#'
#' @param k Bernstein polynomial k
#' @param n Bernstein polynomial degree
#' @param indeterminate indeterminate
#' @return a mpoly object
#' @author David Kahle
#' @name bernstein
#' @examples
#'
#' bernstein(0, 0)
#'
#' bernstein(0, 1)
#' bernstein(1, 1)
#'
#' bernstein(0, 1, "t")
#'
#' bernstein(0:2, 2)
#' bernstein(0:3, 3)
#' bernstein(0:3, 3, "t")
#'
#'
#' bernstein(0:4, 4)
#' bernstein(0:10, 10)
#' bernstein(0:10, 10, "t")
#' bernstein(0:20, 20, "t")
#'
#' \dontrun{  # visualize the bernstein polynomials
#'
#' library(ggplot2); theme_set(theme_classic())
#' library(tidyr)
#'
#' s <- seq(0, 1, length.out = 101)
#' N <- 10 # number of bernstein polynomials to plot
#' (bernPolys <- bernstein(0:N, N))
#'
#' df <- data.frame(s, as.function(bernPolys)(s))
#' names(df) <- c("x", paste0("B_", 0:N))
#' head(df)
#'
#' mdf <- gather(df, degree, value, -x)
#' head(mdf)
#'
#' qplot(x, value, data = mdf, geom = "line", color = degree)
#'
#' }
#'
#' 







#' @rdname bernstein
#' @export
bernstein <- function(k, n, indeterminate = "x"){  
  
  stopifnot(k <= n)
  
  ## make it possible for vector k args
  if(length(k) > 1){
    listOPolys <- lapply(k, function(.) bernstein(., n, indeterminate))
    class(listOPolys) <- "mpolyList"
    return(listOPolys)
  }
  
  ## construct coefficients and degrees of terms
  m <- n - k  
  coefs <- choose(n, k) * (-1)^(0:m) * choose(m, 0:m)
  degs  <- k:n
  
  ## construct polynomial as list
  p <- Map(function(deg, coef) c(x = deg, coef = coef), degs, coefs)
  
  ## wipe out zeros
  p <- lapply(p, function(v) v[v != 0])
  
  ## class list
  class(p) <- c("bernstein", "mpoly")
  attr(p, "bernstein") <- list(k = k, n = n, indeterminate = indeterminate)

  ## swap and return
  swap(p, "x", indeterminate)
}




























