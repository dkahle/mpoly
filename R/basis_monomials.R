#' Enumerate basis monomials
#'
#' Enumerate basis monomials in the standard basis up to a given degree.
#'
#' @param indeterminates a character vector
#' @param d the highest total degree
#' @return a [mpolyList()] object of monomials
#' @export
#' @examples
#'
#' basis_monomials(c("x", "y"), 2)
#' basis_monomials(c("x", "y"), 3)
#' basis_monomials(c("x", "y", "z"), 2)
#' basis_monomials(c("x", "y", "z"), 3)
#' 
basis_monomials <- function(indeterminates, d) {
  
  # to get the x-y example working cleanly
  indeterminates <- rev(indeterminates)
  
  # compute powers as rows of a matrix
  p <- length(indeterminates)
  pows <- lapply(0:d, function(.) burst(n = ., r = p))
  pows <- do.call("rbind", pows)
  
  # make function to convert a power vector to an mpoly
  pow_to_mpoly <- function(v) {
    v <- c(v, 1)
    names(v) <- c(indeterminates, "coef")
    structure(list(v[v != 0]), class = "mpoly")
  }
    
  # split rows across list, add a coef, convert to mpoly
  lapply(
    split(pows, 1:nrow(pows)),
    pow_to_mpoly
  ) ->.;
    unname(.) ->.;
    structure(., class = "mpolyList")
  
}

