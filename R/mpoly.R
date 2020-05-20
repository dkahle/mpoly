#' Define an mpoly object.
#'
#' mpoly is the most basic function used to create objects of class mpoly.
#' However, it is not a general purpose function; for that see [mp()].
#'
#' @param list a list from which to construct an mpoly object
#' @param varorder (optional) a character vector setting the intrinsic variable
#'   order of the polynomial
#' @return Object of class mpoly.
#' @author David Kahle \email{david@@kahle.io}
#' @seealso [mp()]
#' @export
#' @examples
#' 
#' list <- list(
#'   c(coef = 1),
#'   c(x = 1, coef = 1)
#' )
#' mpoly(list)
#' 
#' 
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
  
  flat_list <- unlist(list)
  flat_list <- flat_list[names(flat_list) != "coef"]
  if (!all(is.wholenumber(flat_list)) || any(flat_list < 0)){
    stop("degrees must be nonnegative integers.", call. = FALSE)
  }  
  
  
  # give terms without a coefficient a coef of 1
  add_coef_if_missing <- function(v){
    if (any("coef" == names(v))) return(v)
    c(v, "coef" = 1)
  }
  list <- lapply(list, add_coef_if_missing)
  
  
  
  ## organize 
    
  # remove terms with coef 0
  list <- filter_out_zero_terms(list)
  
  
  # remove 0 degrees, combine like degrees, single coef as rightmost element
  list <- lapply(list, function(v){  	
    
    # separate vardegs from coefs
    coef_ndx <- which(names(v) == "coef")
    coefs <- v[coef_ndx]    
    v     <- v[-coef_ndx]
  	
  	# combine like degrees (sum)
  	if(length(names(v)) != length(unique(names(v)))) v <- fast_named_vec_tapply(v, sum)   
  	
  	# combine like coefficients (product)
  	coefs <- c(coef = prod(coefs))
  	
  	# remove zero degree elements, combine and return  
    c(v[v != 0], coefs)
    
  })
  
  
  
  ## set intrinsic varorder - done again after 0 degrees are removed
  vars <- unique(names(flat_list))
  
  
  # deal with varorder argument
  if (!missing(varorder)) {  	
    if( !setequal(vars, varorder) ){
      stop("If specified varorder must be a permutation of ", stri_c(vars, collapse = ", "), call. = FALSE)	
    }    
    vars <- varorder
  }

  
  # sort variables in terms
  list <- lapply(list, function(v){
    p <- length(v) - 1L
    if(p == 0L) return(v)
    c( (v[1:p])[intersect(vars, names(v[1:p]))], v["coef"] )
  })
  
   
   
  ## prepare to check if like terms are present
  monomials <- vapply(list, function(v){
  	p <- length(v) - 1 # remove coef on monomials
    paste(names(v[1:p]), v[1:p],  sep = "", collapse = "")
  }, character(1))  
  # e.g. c("x1", "y1", "y1", "x2z1", "y4t3", "x1", "x1", "coef5")
  
  unique_monomials <- unique(monomials)
  
  
  
  ## check if like terms are present and, if so, correct  
  if (length(monomials) != length(unique_monomials)) {

    matched_monomials <- match(monomials, unique_monomials)
    matched_monomials <- factor(matched_monomials, levels = 1:max(matched_monomials))
    ndcs2combine      <- split.default(1:length(list), matched_monomials)
    
    list <- lapply(ndcs2combine, function(v){
      if(length(v) == 1) return(list[[v]])      
      coef <- sum(vapply(list[v], `[`, double(1), length(list[[v[1]]])))
      v <- list[[v[1]]]
      v["coef"] <- coef
      v
    })
    
    names(list) <- NULL # i.e. list <- unname(list)
  }
  
  
  
  ## combine constant terms
  ## mpoly(list(c(x = 1, coef = 1), c(coef = 1), c(coef = 2)))    
  non_constant_terms <- fast_filter(is_not_length_one, list)
  constant_terms     <- fast_filter(is_length_one, list)
  
  if (length(constant_terms) > 0) {
    list <- c(
      non_constant_terms, 
      list(Reduce(`+`, constant_terms))
    )  
  }
  
  
  
  ## re-organize after like-terms combined
  list <- filter_out_zero_terms(list)

  
  
  ## return classed list
  class(list) <- "mpoly"
  list  
}











filter_out_zero_terms <- function(list){
  
  # a term is zero if any of its coefficients are 0
  has_non_zero_coef <- function(v) all(v["coef"] != 0)
  
  # remove terms with coef 0
  list <- fast_filter(has_non_zero_coef, list)
  
  # if all terms have been eliminated, recreate it with a 0 coef
  if(length(list) == 0) list <- list(c(coef = 0))  
  
  # return
  list  
}







is_length_one    <- function(x) length(x) == 1L
is_not_length_one <- function(x) length(x) > 1L
fast_filter     <- function(f, x) x[vapply(x, f, logical(1))]







fast_named_vec_tapply <- function(x, f, type = double(1)){
  uniqueNames   <- unique(names(x))
  matched_names <- match(names(x), uniqueNames) # indices
  matched_names <- factor(matched_names, levels = 1:max(matched_names))
  groupIndices  <- split.default(1:length(x), matched_names)
  out <- vapply(groupIndices, function(ndcs) f(x[ndcs]), type)
  names(out) <- uniqueNames
  out
}
# x <- 1:10
# names(x) <- sample(letters[1:3], 10, replace = TRUE)
# fast_named_vec_tapply(x, sum)
# tapply(x, names(x), sum)



