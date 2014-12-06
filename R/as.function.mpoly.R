#' Change a multivariate polynomial into a function.
#'
#' Transforms an mpoly object into a function which can be evaluated.
#'
#' @param x an object of class mpoly
#' @param varorder the order in which the arguments of the function will be provided 
#' @param vector whether the function should take a vector argument (TRUE) or a series of arguments (FALSE)
#' @param ... any additional arguments
#' @usage \method{as.function}{mpoly}(x, varorder = vars(x), vector = TRUE, ...)
#' @export
#' @examples
#' mpoly <- mp('1 2 3 4')
#' f <- as.function(mpoly)
#' f(10) # -> 24
#' mpoly <- mp('x + 3 x y + z^2 x')
#' f <- as.function(mpoly)
#' f(1:3) # -> 16
#' f(c(1,1,1)) # -> 5
#'
#' f <- as.function(mpoly, vector = FALSE)
#' f(1, 2, 3) # -> 16
#' f(1, 1, 1) # -> 5
#' 
#' f <- as.function(mpoly, varorder = c('z','y','x'), vector = FALSE)
#' f(3, 2, 1) # -> 16
#' f(1, 1, 1) # -> 5
#'
as.function.mpoly <- function(x, varorder = vars(x), vector = TRUE, ...){
	
  # argument checking
  stopifnot(is.character(varorder))
  stopifnot(is.logical(vector))  	

  if(!is.mpoly(x)){
    stop('x must be of class mpoly.', call. = FALSE)
  }
	
  if(!setequal(varorder, vars(x))){
    stop('varorder must contain all of the variables of x.',
      call. = FALSE)
  }
  
  p <- length(vars(x))
  
  if(length(vars) == 0){ # constant function
    return(function(x){ return(unclass(x)[[1]]['coef']) })
  }
    
  # univariate polynomial
  if(p == 1) vector <- FALSE
  
  # general polynomials as a vector argument
  if(vector){
    mpoly_string <- suppressMessages(print.mpoly(x, stars = TRUE))
    mpoly_string <- paste(' ', mpoly_string, ' ', sep = '')
    for(k in 1:p){
      mpoly_string <- gsub(
        paste(' ', varorder[k], ' ', sep = ''),
        paste(' .[', k, '] ', sep = ''),
        mpoly_string
      )
      mpoly_string <- gsub(
        paste(' ', varorder[k], '\\*\\*', sep = ''),
        paste(' .[', k, ']**', sep = ''),
        mpoly_string
      )      
    }
    v <- paste('(', paste(varorder, collapse = ', '), ')', sep = '')
    message('f(.) with . = ', v)
    mpoly_string <- paste('function(.){', mpoly_string, '}')    
    return(eval(parse(text = mpoly_string)))
  }
  
  # general polynomials as a bunch of arguments
  if(!vector){
    mpoly_string <- suppressMessages(print.mpoly(x, stars = TRUE))
    message('f(', paste(varorder, collapse = ', '), ')', sep = '')
    mpoly_string <- paste(
      'function(', 
      paste(varorder, collapse = ', '),
      '){', 
      mpoly_string, 
      '}',
      sep = ''
    )
    return(eval(parse(text = mpoly_string)))
  }  
  
}
