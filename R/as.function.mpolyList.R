#' Change a vector of multivariate polynomials into a function.
#'
#' Transforms an mpolyList object into a function which can be evaluated.
#'
#' @param x an object of class mpolyList
#' #@param varorder the order in which the arguments of the function will be provided (default vars(mpoly))
#' #@param vector whether the function should take a vector argument (TRUE) or a series of arguments (FALSE)
#' @param ... any additional arguments
#' @usage \method{as.function}{mpolyList}(x, varorder = vars(x), vector = TRUE, ...)
#' @export
#' @examples
#' mpolyList <- mp(c('2 x + 1', 'x - z^2'))
#' f <- as.function(mpolyList)
#' f(c(1,2)) # -> (2*1 + 1, 1-2^2) = 3 -3
#' 
#' f <- as.function(mpolyList, varorder = c('x','y','z'))
#' f(c(1,0,2)) # -> 3 -3
#' f(c(1,4,2)) # -> 3 -3
#' 
#' f <- as.function(mpolyList, varorder = c('x','y','z'), vector = FALSE)
#' f(1, 0, 2) # -> 3 -3
#' f(1, 4, 2) # -> 3 -3
#' 
#' 
#' mpoly <- mp('x + y^2 + y z')
#' mpolyList <- gradient(mpoly)
#' f <- as.function(mpolyList, varorder = vars(mpoly))
#' f(c(0,2,3)) # -> 1 7 2
#' # nice for supplying gradient functions
#'
#'
as.function.mpolyList <- function(x, varorder = vars(x), vector = TRUE, ...){
  
  # argument checking
  stopifnot(is.character(varorder))
  stopifnot(is.logical(vector))  	

  if(!is.mpolyList(x)){
    stop('x must be of class mpolyList.', call. = FALSE)
  }
	
  if(!missing(varorder) && !all( vars(x) %in% varorder )){
    stop('varorder must contain all of the variables of mpoly.',
      call. = FALSE)
  }
  
  p <- length(varorder)
    
  # univariate polynomial
  if(p == 1) vector <- FALSE
  
  # general polynomials as a vector argument
  if(vector){
    mpoly_string <- suppressMessages(print.mpolyList(x, stars = TRUE))
    mpoly_string <- paste(' ', mpoly_string, ' ', sep = '', collapse = ',')
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
    mpoly_string <- paste('function(.){ c(', mpoly_string, ') }')    
    return(eval(parse(text = mpoly_string)))
  }
  
  # general polynomials as a bunch of arguments
  if(!vector){
    mpoly_string <- suppressMessages(print.mpolyList(x, stars = TRUE))
    mpoly_string <- paste(mpoly_string, collapse = ', ')
    message('f(', paste(varorder, collapse = ', '), ')', sep = '')
    mpoly_string <- paste(
      'function(', 
      paste(varorder, collapse = ', '),
      '){ c(', 
      mpoly_string, 
      ') }',
      sep = ''
    )
    return(eval(parse(text = mpoly_string)))
  }  
  
}
