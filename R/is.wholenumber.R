#' Test whether an object is a whole number.
#'
#' Test whether an object is a whole number.
#'
#' @param x object to be tested
#' @param tol tolerance within which a number is said to be whole
#' @return Vector of logicals.
#' @export
#' @examples
#' is.wholenumber(seq(-3,3,.5))
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5){
  abs(x - round(x)) < tol
}

