#' Compute partial derivatives of a multivariate polynomial.
#' 
#' This is a deriv method for mpoly objects.  It does not call the
#' deriv function (from package stats).
#' 
#' @param expr an object of class mpoly
#' @param var character - the partial derivative desired
#' @param ... any additional arguments
#' @return An object of class mpoly or mpolyList.
#' @export
#' @examples
#' m <- mp('x y + y z + z^2')
#' deriv(m, 'x')
#' deriv(m, 'y')
#' deriv(m, 'z')
#' deriv(m, c('x','y','z'))
#' deriv(m, 'a')
#' is.mpoly(deriv(m, 'x'))
#' is.mpolyList( deriv(m, c('x','y','z')) )
deriv.mpoly <- function(expr, var, ...){

  if(missing(var)){
  	stop('var must be specified, see ?deriv.mpoly', call. = FALSE)
  }
	
  # argument checks	
  if(!is.mpoly(expr)){
    stop('expr must be of class mpoly.', call. = FALSE)
  }
  stopifnot(is.character(var))
  
  
  # if many vars provided
  if(length(var) > 1){
    mpolyList <- lapply(as.list(var), function(var){
      deriv(expr, var = var, ...)
    })
    class(mpolyList) <- 'mpolyList'  
    return(mpolyList)    
  }
  
	
  # if the variable is not in the polynomial, return zero
  if(!(var %in% vars(expr))){
    return( mpoly(list(c(coef = 0))) )
  }
	
  expr <- unclass(expr)
  
  # take derivative
  expr <- lapply(expr, function(v){
  	if(length(v) == 1) return(c(coef = 0))
    p <- length(v)
    if(!(var %in% names(v[1:p]))) return(c(coef = 0))
    v['coef'] <- unname(v[var]) * v['coef']
    v[var] <- v[var] - 1
    v
  })
  
  # return
  mpoly(expr)

}


#' Compute gradient of a multivariate polynomial.
#'
#' This is a wrapper for deriv.mpoly.
#'
#' @param mpoly an object of class mpoly
#' @seealso \code{\link{deriv.mpoly}}
#' @return An object of class mpoly or mpolyList.
#' @export
#' @examples
#' m <- mp('x y + y z + z^2')
#' gradient(m)
gradient <- function(mpoly){
  deriv(mpoly, var = vars(mpoly))
}


