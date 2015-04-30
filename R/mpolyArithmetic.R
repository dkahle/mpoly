#' Arithmetic with multivariate polynomials
#'
#' Arithmetic with multivariate polynomials
#' 
#' @param e1 an object of class mpoly
#' @param e2 an object of class mpoly
#' @return object of class mpoly
#' @name mpolyArithmetic
#' @examples
#' 
#' p <- mp("x + y")
#' p + p
#' p - p
#' p * p
#' p^2
#' 
#' 
#' p + 1
#' 2*p
#' 
#' 
#' 
NULL








#' @rdname mpolyArithmetic
#' @export
`+.mpoly` <- function(e1, e2){
	
  mpoly( c( unclass(e1), unclass(e2) ) )
 
}







#' @rdname mpolyArithmetic
#' @export
`-.mpoly` <- function(e1, e2){
  
  ## flip coefficients of each term
  e2 <- lapply(e2, function(v){
    v["coef"] <- -v["coef"]
    v
  })
  class(e2) <- "mpoly"
  
  ## add
  e1 + e2	
}






#' @rdname mpolyArithmetic
#' @export
`*.mpoly` <- function(e1, e2){
  
  ## allow for multiplication by a constant
  if(is.numeric(e1) && length(e1) == 1) e1 <- mpoly(list(c(coef = e1)))
  if(is.numeric(e2) && length(e2) == 1) e2 <- mpoly(list(c(coef = e2)))
  
  
  
  ## argument check
  stopifnot(is.mpoly(e1))
  stopifnot(is.mpoly(e2))
  
  
  
  ## multiply
  list <- lapply(e1, function(v){
    lapply(e2, function(z){
      c(v, z)  
    })	
  })
  
  
  
  ## return
  mpoly( unlist(list, recursive = FALSE) )
}	


l <- list(
  c(x = 1, coef = 1, x = 1, coef = 1)  
)









#' @rdname mpolyArithmetic
#' @export
'^.mpoly' <- function(e1, e2){
  
  if(!is.mpoly(e1)) stop('e1 must be of class mpoly.', call. = FALSE)
  
  if(!is.wholenumber(e2) || e2 < 0) stop('exponent must be a positive integer.', call. = FALSE)
  
  if(e2 == 0) return(mpoly(list(c(coef = 1))))
  
  out <- mpoly(list(c(coef = 1)))
  for(k in 1:e2) out <- out * e1
  
  out
}
