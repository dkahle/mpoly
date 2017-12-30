#' Define an mpoly object.
#' 
#' mpoly is the most basic function used to create objects of class
#' mpoly. However, it is not a general purpose function; for that
#' see mp.
#' 
#' @param list a list from which to construct an mpoly object
#' @param varorder (optional) a character vector setting the
#'   intrinsic variable order of the polynomial
#' @return Object of class mpoly.
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @seealso \code{\link{mp}}
#' @export
#' @examples
#' list <- list(
#'   c(x = 1, coef = 1, y = 0),
#'   c(x = 0, y = 1, coef = 2),  
#'   c(y = 1, coef = -6),  
#'   c(z = 1, coef = -3, x = 2),  
#'   c(x = 1, coef = 0, x = 3),
#'   c(t = 1, coef = 4, t = 2, y = 4),
#'   c(x = 1),
#'   c(x = 1),
#'   c(coef = 5),
#'   c(coef = 5),
#'   c(coef = -5)
#' )
#' 
#' mpoly(list) # 3 x  -  4 y  -  3 x^2 z  +  4 y^4 t^3  +  5
#' mpoly(list, varorder = c("y", "z", "t", "x"))
#' 
#' list <- list(  c(x = 5, x = 2, coef = 5, coef = 6, y = 0) )
#' mpoly(list)
#' 
#' 
mpoly <- function(list, varorder){

  ## argument checking
  if(!is.list(list)) stop("input to mpoly must be a list.", call. = FALSE)
  
  if(!all(vapply(list, is.numeric, logical(1)))){
    stop("each element of list must be of type numeric.", call. = FALSE)  
  }
  
  if(any( unlist(lapply(list, function(v) nchar(names(v))), use.names = FALSE) == 0 )){
    stop("each element of list must be named for every element.", call. = FALSE)  
  }    
  
  flatList <- unlist(list)
  flatList <- flatList[names(flatList) != "coef"]
  if(!all(is.wholenumber(flatList)) || any(flatList < 0)){
    stop("degrees must be nonnegative integers.", call. = FALSE)
  }
  
  ## set intrinsic varorder - done again after 0 degrees are removed
  vars <- unique(names(flatList))
  
  
  # deal with varorder argument
  if(!missing(varorder)){  	
    if( !setequal(vars, varorder) ){
      stop(paste(
        "if specified varorder must be a permutation of",
        paste(vars, collapse = ", ")        
      ), call. = FALSE)	
    }    
    vars <- varorder
  }
  
  list <- reducePoly(list, vars)
  
  ## return classed list
  class(list) <- "mpoly"
  list  
}
