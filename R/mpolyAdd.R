 #' Add multivariate polynomials.
#'
#' Compute the sum of two multivariate polynomials.
#' 
#' @param e1 an object of class mpoly
#' @param e2 an object of class mpoly
#' @method + mpoly
#' @aliases +.mpoly
#' @return object of class mpoly
#' @name mpolyAdd
#' @export +.mpoly -.mpoly
#' @examples
#' 
#' list <- list(
#'   c(x = 1, coef = 1, y = 0),
#'   c(x = 0, y = 1, coef = 2),  
#'   c(y = 1, coef = -6),  
#'   c(z = 1, coef = -3, x = 2),  
#'   c(x = 1, coef = 0, x = 3),
#'   c(t = 1, coef = 4, t = 2, y = 4),
#'   c(coef = 5),
#'   c(coef = -6)
#' )
#' 
#' p <- mpoly(list)
#' p + p
#'
#' list <- list(c(coef = 1))
#' ( p1 <- mpoly(list) )
#' list <- list(c(coef = -1))
#' ( p2 <- mpoly(list) )
#' p1 + p2
'+.mpoly' <- function(e1, e2){
	
  mpoly( c( unclass(e1), unclass(e2) ) )
 
}





#' Subtract multivariate polynomials.
#'
#' Compute the difference of two multivariate polynomials.
#' 
#' @param e1 an object of class mpoly
#' @param e2 an object of class mpoly
#' @method - mpoly
#' @aliases -.mpoly
#' @return object of class mpoly
#' @name mpolySub
#' @examples
#' 
#' list <- list(
#'   c(x = 1, coef = 1, y = 0),
#'   c(x = 0, y = 1, coef = 2),  
#'   c(y = 1, coef = -6),  
#'   c(z = 1, coef = -3, x = 2),  
#'   c(x = 1, coef = 0, x = 3),
#'   c(t = 1, coef = 4, t = 2, y = 4),
#'   c(coef = 5),
#'   c(coef = -6)
#' )
#' 
#' p <- mpoly(list)
#' p - p
#'
#' list <- list(c(coef = 1))
#' ( p1 <- mpoly(list) )
#' list <- list(c(coef = -1))
#' ( p2 <- mpoly(list) )
#' p1 - p2
'-.mpoly' <- function(e1, e2){
  e2 <- lapply(e2, function(v){
    v['coef'] <- -v['coef']
    v
  })
  class(e2) <- 'mpoly'
  e1 + e2	
}







