#' Multiply multivariate polynomials.
#'
#' Compute the product of two mpoly objects.
#' 
#' @param e1 an object of class mpoly
#' @param e2 an object of class mpoly
#' @method * mpoly
#' @aliases *.mpoly
#' @return An object of class mpoly.
#' @name mpolyProd
#' @export
#' @examples
#' 
#' x <- mpoly(list(
#'   c(x = 1, coef = 1),
#'   c(coef = 1)
#' ))
#' y <- mpoly(list(
#'   c(x = 1, coef = 1),
#'   c(coef = 1)
#' ))
#' x * y
#' 5 * x
#' -1 * x
#' 
#' 
#' list <- list(
#'   c(x = 1, coef = 1),
#'   c(y = 1, coef = -4),  
#'   c(x = 2, z = 1, coef = -3),
#'   c(t = 3, y = 4, coef = 2)    
#' )
#' x <- mpoly(list)
#' y <- mpoly(list(c(x = 1, coef = 1), c(coef = 1)))
#' ( p <- x * y )
#' reorder(p, order = 'lex', varorder = c('t','x','y','z'))
#' 
#' 
#' 
'*.mpoly' <- function(e1, e2){
  
  ## allow for multiplication by a constant
  if(is.numeric(e1) && length(e1) == 1){
    e1 <- mpoly(list(c(coef = e1)))
  }
  
  if(is.numeric(e2) && length(e2) == 1){
    e2 <- mpoly(list(c(coef = e2)))
  }  
  
  
  
  ## argument check
  if(!is.mpoly(e1) || !is.mpoly(e2)){
    stop('e1 and e2 must be of class mpoly.', call. = FALSE)
  }
  
  
  
  ## multiply
  list <- lapply(e1, function(v){
    lapply(e2, function(z){
      c(v, z)	
    })	
  })
 
 
   
  ## return
  mpoly( unlist(list, recursive = FALSE) )
}	






