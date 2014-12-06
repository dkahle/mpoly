#' Define an mpoly object.
#'
#' mpoly is the most basic function used to create objects of class mpoly. 
#' However, it is not a general purpose function; for that see mp. 
#' 
#' @param list a list from which to construct an mpoly object
#' @param varorder (optional) a character vector setting the intrinsic variable order of the polynomial 
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
#'   c(coef = 5),
#'   c(coef = -5)
#' )
#' 
#' mpoly(list)
#' mpoly(list, varorder = c('y', 'z', 't', 'x'))
#' 
#' list <- list(  c(x = 5, x = 2, coef = 5, coef = 6, y = 0) )
#' mpoly(list)
#' 
#' # ERROR CHECKS (in order)
#' # mpoly(1:5)
#' # mpoly(list(c(x = 'a', coef = 1)))
#' # mpoly(list(c(x = 1,     1, coef = 2)))
#' # mpoly(list(c(x = 1, y = 1)))
#' # mpoly(list(c(x = 1, y = 1.5, coef = 2)))
#' # mpoly(list(c(x = 1, y = -2, coef = 2)))
#' # mpoly(list(c(x = 1, y = 2, coef = 2)), varorder = 'x')
#' 
mpoly <- function(list, varorder){

  ## argument checking
  if(!is.list(list)){
    stop('list must be of class list.', call. = FALSE)
  }
  
  if(!all(sapply(list, is.numeric))){
    stop('each element of list must be of type numeric.', call. = FALSE)  
  }
  
  if(any( unlist(lapply(list, function(v) nchar(names(v)))) == 0 )){
    stop('each element of list must be named for every element.', call. = FALSE)  
  }  
  
  if(!all( sapply(list, function(v) 'coef' %in% names(v)) )){
    stop('each element of list must contain a element named coef.', call. = FALSE)    
  }
  
  flatList <- unlist(list)
  flatList <- flatList[names(flatList) != 'coef']
  if(!all(is.wholenumber(flatList)) || any(flatList < 0)){
    stop('degrees must be nonnegative integers.', call. = FALSE)
  }  
  
  
  
  
  ## organize 
  
  # remove terms with coef 0
  terms2keep <- !sapply(list, function(v) v['coef'] == 0)
  list <- list[terms2keep]
  if(length(list) == 0) list <- list(c(coef = 0))
  
  # remove 0 degrees, combine like degrees, put 'coef' as rightmost element
  list <- lapply(list, function(v){
  	
    # separate vardegs from coefs
    coef_ndx <- which(names(v) == 'coef')
    coefs <- v[coef_ndx]    
    v <- v[-coef_ndx]

  	
  	# combine like degrees (sum)
  	if(length(names(v)) != length(unique(names(v)))){
  	  split_v <- split(v, match(names(v), unique(names(v))))
  	  names(split_v) <- sapply(split_v, function(v) names(v)[1])
      v <- sapply(split_v, sum)
  	}
  	
  	# combine like coefficients (product)
  	coefs <- c(coef = prod(coefs))
  	
  	# remove zero degree elements
    if(length(v) > 0){  	
  	  p <- length(v)
      v <- v[1:p][v[1:p] != 0]
    }    
    c(v, coefs)
  })
  
  
  
  
  ## set intrinsic varorder - done again after 0 degrees are removed
  flatList <- unlist(list)
  flatList <- flatList[names(flatList) != 'coef']
  vars <- unique(names(flatList))
  
  # deal with varorder argument
  if(!missing(varorder)){
  	
    if( !setequal(vars, varorder) ){
      stop(paste(
        'if specified varorder must be a permutation of',
        paste(vars, collapse = ', ')        
      ), call. = FALSE)	
    }
    
    vars <- varorder
  }

  # sort
  list <- lapply(list, function(v){
    p <- length(v) - 1
    c( (v[1:p])[intersect(vars, names(v[1:p]))], v['coef'] )
  })   
  
  
  
  
  ## combine like terms, if present  
  monomials <- sapply(list, function(v){
  	p <- length(v) - 1
    paste(names(v[1:p]), v[1:p],  sep = '', collapse = '')
  })
  if(length(monomials) != length(unique(monomials))){
    ndcs2combine <- split(1:length(list), match(monomials, unique(monomials)))
    list <- lapply(ndcs2combine, function(v){
      flatList <- unlist(list[v])
      coef <- sum(flatList[names(flatList) == 'coef'])
      v <- list[v][[1]]
      v['coef'] <- coef
      v
    })
    list <- unname(list)
  }
  
  # combine constant terms
  constant_ndcs <- which(sapply(list, length) == 1)
  if(length(constant_ndcs) > 1){
    list <- c(
      list[-constant_ndcs],
      c(sum(unlist(list[constant_ndcs])))
    )
    names(list[[length(list)]]) <- 'coef'
  }
  
  
  
  
  ## re-organize after like-terms combined
  
  # remove terms with coef 0
  terms2keep <- !sapply(list, function(v) v['coef'] == 0)
  list <- list[terms2keep]  
  if(length(list) == 0) list <- list(c(coef = 0))  

  
  
  
  ## return classed list
  class(list) <- 'mpoly'
  list  
}




