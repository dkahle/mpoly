#' Extract exponents
#' 
#' Extract exponents of a multivariate polynomial.
#' 
#' @param x an \code{\link{mpoly}} object
#' @param reduced if TRUE, don't include zero degrees
#' @return a list of named vectors of degrees
#' @export
#' @examples
#' x <- mp("x^4 + y + 2 x y^2 - 3 z")
#' exponents(x)
#' exponents(x, reduce = TRUE)
#' lapply(exponents(x), is.integer)
#' 
exponents <- function(x, reduced = FALSE){

  l <- lapply(x, function(term){
    fixed_term <- as.integer(term[-which(names(term) == "coef")])
    names(fixed_term) <- names(term[-which(names(term) == "coef")])
    fixed_term
  })  
  
  if(reduced) return(l)
  
  v <- vars(x)
  p <- length(v)
  tmp <- rep.int(0L, p)
  names(tmp) <- v
  
  lapply(l, function(exp){
    tmp <- tmp + exp[v]
    tmp[is.na(tmp)] <- 0
    tmp <- as.integer(tmp)
    names(tmp) <- v
    tmp
  })
}