#' Solve a univariate mpoly with polyroot
#'
#' Solve a univariate mpoly with polyroot
#'
#' @param mpoly an mpoly
#' @param real_only return only real solutions?
#' @export
#' @examples
#'
#' solve_unipoly(mp("x^2 - 1")) # check x = -1, 1
#' solve_unipoly(mp("x^2 - 1"), real_only = TRUE)
#' 
solve_unipoly <- function(mpoly, real_only = FALSE) {
  mpoly <- reorder(mpoly)
  degs <- vapply(mpoly, function(term) {
    if(length(term) == 1) return(0L)
    term[[1]]
  }, double(1))
  coefs <- sapply(mpoly, `[[`, "coef") # vapply errors?
  coef_vec <- rep(0L, degs[1]+1)
  names(coef_vec) <- 0:degs[1]
  for(k in 1:length(mpoly)) coef_vec[as.character(degs[k])] <- coefs[k]
  solns <- polyroot(coef_vec)
  if (real_only) {
    is.real <- function(x) Im(x) == 0
    solns <- Re(Filter(is.real, solns))
  }
  solns
}

