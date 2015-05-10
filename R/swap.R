#' Swap polynomial indeterminates
#' 
#' Swap polynomial indeterminates
#' 
#' @param p polynomial
#' @param variable the variable in the polynomial to replace
#' @param replacement the replacement variable
#' @return a mpoly object
#' @author David Kahle 
#' @export
#' @examples
#' 
#' (p <- mp("(x + y)^2"))
#' swap(p, "x", "t")
#' 
#'
swap <- function(p, variable, replacement){
  
  ## arg checks
  stopifnot(variable %in% vars(p))
  if(replacement %in% vars(p)){
    stop("the replacement value cannot be a variable in the polynomial, try plug.", call. = FALSE)
  }
  
  ## swapping
  p <- unclass(p)
  p <- lapply(p, function(v){
    names(v)[names(v) == variable] <- replacement
    v
  })
  class(p) <- "mpoly"
  p
}



