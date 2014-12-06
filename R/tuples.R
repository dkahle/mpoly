#' Determine all n-tuples using the elements of a set.
#'
#' Determine all n-tuples using the elements of a set.
#' 
#' @param set a set
#' @param n length of each tuple
#' @param repeats if set contains duplicates, should the result?
#' @return a matrix whose rows are the n-tuples
#' @export
#' @examples
#' tuples(1:2, 5)
#' apply(tuples(c('x','y','z'), 3), 1, paste, collapse = '')
#'
#' # multinomial coefficients
#' r <- 2 # number of variables, e.g. x, y
#' n <- 2 # power, e.g. (x+y)^2
#' apply(burst(n,r), 1, function(v) factorial(n)/ prod(factorial(v))) # x, y, xy
#' mp('x + y')^n
#'
#' r <- 2 # number of variables, e.g. x, y
#' n <- 3 # power, e.g. (x+y)^3
#' apply(burst(n,r), 1, function(v) factorial(n)/ prod(factorial(v))) # x, y, xy
#' mp('x + y')^n
#'
#' r <- 3 # number of variables, e.g. x, y, z
#' n <- 2 # power, e.g. (x+y+z)^2
#' apply(burst(n,r), 1, function(v) factorial(n)/ prod(factorial(v))) # x, y, z, xy, xz, yz
#' mp('x + y + z')^n
tuples <- function(set, n = length(set), repeats = FALSE){
  r <- length(set)	
  set2 <- 1:r
  sets4call <- paste(rep('set2', length = n), collapse = ', ')
  str4eval <- paste('expand.grid(', sets4call, 
    ', stringsAsFactors=F, KEEP.OUT.ATTRS=F)', sep = '')
  mat <- as.matrix(unname(eval(parse(text = str4eval))))
  d <- dim(mat)
  mat <- paste('_', mat, sep = '')
  dim(mat) <- d
  for(k in 1:r){
    mat[mat == paste('_', k, sep='')] <- set[k]
  }
  if(is.numeric(set)){
    mat <- as.numeric(mat)
    dim(mat) <- d
  }
  if(!repeats) mat <- unique(mat)
  mat
}

