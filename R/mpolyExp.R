#' Exponentiate multivariate polynomials.
#'
#' Exponentiate multivariate polynomials.
#' 
#' @param e1 an object of class mpoly
#' @param e2 a positive integer
#' @method ^ mpoly
#' @aliases ^.mpoly
#' @return object of class mpoly
#' @name mpolyExp
#' @export
#' @examples
#' list <- list(
#'   c(x = 1, coef = 1),
#'   c(y = 1, coef = -4),  
#'   c(x = 2, z = 1, coef = -3),
#'   c(t = 3, y = 4, coef = 2)    
#' )
#' mp1 <- mpoly(list)
#' mp1 * mp1
#' mp1^2
#' mp1^10
'^.mpoly' <- function(e1, e2){
  
  if(!is.mpoly(e1)){
    stop('e1 must be of class mpoly.', call. = FALSE)
  }
  
  if(!is.wholenumber(e2) || e2 < 0){
    stop('exponent must be a positive integer.', call. = FALSE)
  }
  
  if(e2 == 0) return(mpoly(list(c(coef = 1))))
  
  out <- mpoly(list(c(coef = 1)))
  for(k in 1:e2){
  	out <- out * e1
  }
  out
}
