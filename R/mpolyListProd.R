#' Multiply vectors multivariate polynomials element-wise.
#'
#' Compute the product of two mpolyList objects.
#' 
#' @param e1 an object of class mpoly
#' @param e2 an object of class mpoly
#' @method * mpolyList
#' @aliases *.mpolyList
#' @return An object of class mpoly.
#' @name mpolyListProd
#' @export
#' @examples
#' ( ms1 <- mp( c('x + 1', 'x + 2') ) )
#' ( ms2 <- mp( c('x - 1', 'x - 2') ) )
#' ms1 * ms2
#' # mpoly * mpolyList not available.
'*.mpolyList' <- function(e1, e2){
  
  ## argument check
  
  if(is.numeric(e1) && length(e1) == 1){
    e1 <- mpoly(list(c(coef = e1)))
  }
  
  if(is.mpoly(e1)){
  	e1 <- mpolyList(e1)
  }
  
  if(is.numeric(e2) && length(e2) == 1){
    e2 <- mpoly(list(c(coef = e2)))
  }  
  
  if(is.mpoly(e2)){
  	e2 <- mpolyList(e2)
  }  
  
  if(!is.mpolyList(e1) || !is.mpolyList(e2)){
    stop('e1 and e2 must be of class mpolyList.', call. = FALSE)
  }
  
  if(length(e1) != length(e2)){
    stop('e1 and e2 must have equal length.', call. = FALSE)
  }
  
  
  
  ## determine length, flatten, and make indices on which to multiply
  n <- length(e1)
  
  flatList <- unlist(list(
    unclass(e1),
    unclass(e2)
  ), recursive = FALSE)
  
  ndcs2add <- split(cbind(1:n, (n+1):(2*n)), 1:n)
  
  
  
  
  ## multiply
  out <- lapply(ndcs2add, function(v){
    Reduce('*', flatList[v])
  })
  out <- unname(out)
  
  
  
  ## caste and return
  class(out) <- 'mpolyList'
  out
}	




