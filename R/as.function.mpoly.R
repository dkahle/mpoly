#' Change a multivariate polynomial into a function.
#'
#' Transforms an mpoly object into a function which can be evaluated.
#'
#' @inheritParams print.mpoly
#' @param vector whether the function should take a vector argument (TRUE) or a
#'   series of arguments (FALSE)
#' @param ... any additional arguments
#' @param squeeze minify code in the created function
#' @usage \method{as.function}{mpoly}(x, varorder = vars(x), vector = TRUE,
#'   silent = FALSE, ..., plus_pad = 1L, times_pad = 1L, squeeze = TRUE)
#' @seealso [plug()], [as.function.mpolyList()]
#' @export
#' @examples
#' 
#' p <- mp("(x - 1)^2")
#' (f <- as.function(p))
#' f(1)
#' f(seq(0, 2, .1))
#'
#' p <- mp("x + 3 x y + z^2 x")
#' (f <- as.function(p))
#' f(1:3) # -> 16
#' f(c(1,1,1)) # -> 5
#'
#' f <- as.function(p, vector = FALSE)
#' f(1, 2, 3) # -> 16
#' f(1, 1, 1) # -> 5
#'
#' f <- as.function(p, varorder = c("z","y","x"), vector = FALSE)
#' f(3, 2, 1) # -> 16
#' f(1, 1, 1) # -> 5
#'
#' # for univariate mpolys, as.function() returns a vectorized function
#' # that can even apply to arrays
#' p <- mp("x^2")
#' f <- as.function(p)
#' f(1:10)
#' (mat <- matrix(1:4, 2))
#' f(mat)
#'
#'
#' p <- mp("1 2 3 4")
#' f <- as.function(p)
#' f(10) # -> 24
#'
#' bernstein(1, 2)
#' s <- seq(0, 1, .01)
#' as.function(bernstein(1, 2))(s)
#' plot(
#'   s,
#'   as.function(bernstein(1, 2))(s)
#' )
#'
#'
#' as.function(mp("x + xx"))
#' as.function(mp("x + xx"), squeeze = FALSE)
#'
#' 
as.function.mpoly <- function(x, varorder = vars(x), vector = TRUE, silent = FALSE, ..., plus_pad = 1L, times_pad = 1L, squeeze = TRUE){
	
  # argument checking
  stopifnot(is.character(varorder))
  stopifnot(is.logical(vector))  	
  stopifnot(is.mpoly(x))
	
  if (!setequal(varorder, vars(x))) stop("varorder must contain all of the variables of x.", call. = FALSE)
  
  
  # determine the number of variables
  p <- length(vars(x))
 
  
  # deal with constant polynomials
  if (is.constant(x)) return( function(.) 0*. + unlist(x)[["coef"]] )
  
  
  # print poly with stars
  mp_str <- print.mpoly(x, stars = TRUE, silent = TRUE, plus_pad = plus_pad, times_pad = times_pad)
  
  
  # univariate polynomial
  if (p == 1L) {
    mp_str <- stri_replace_all_fixed(mp_str, vars(x), ".")
    if (squeeze) mp_str <- stri_replace_all_fixed(mp_str, " ", "")
    if (!silent) message("f(.) with . = ", vars(x))
    f <- function(){}
    formals(f) <- alist(. = )
    body(f) <- as.call(c(
      as.name("{"),
      expression(if(length(.) > 1){
        .[] <- sapply(., f)
        return(.)
      }),
      parse(text = mp_str)
    ))
    return(f)
  }
  
  
  # general polynomials as a vector argument
  if (vector) { 
    mp_str <- stri_c(" ", mp_str, " ") # pad to make parsing easier 
    for (k in 1L:p) {
      mp_str <- stri_replace_all_fixed(mp_str, "**", " ^")
      mp_str <- stri_replace_all_fixed(mp_str, stri_c(" ", varorder[k], " "), stri_c(" .[", k, "] "))
      mp_str <- stri_replace_all_fixed(mp_str, " ^", "**")
    } 
    if (squeeze) mp_str <- stri_replace_all_fixed(mp_str, " ", "") 
    v <- stri_c("(", stri_c(varorder, collapse = ", "), ")") 
    if(!silent) message("f(.) with . = ", v) 
    mp_str <- stri_c("function(.) {", mp_str, "}")     
    return(eval(parse(text = mp_str)))
  }
  
  
  # general polynomials as a bunch of arguments
  if (!vector) { 
    if(!silent) message("f(", stri_c(varorder, collapse = ", "), ")", sep = "") 
    mp_str <- stri_c(
      "function(", stri_c(varorder, collapse = ", "), ") {", 
        mp_str, 
      "}",
      sep = ""
    ) 
    return(eval(parse(text = mp_str)))
  }  
 
   
}










as.function.bernstein <- function(x, ...){
 
  # grab bernstein values
  k <- attr(x, "bernstein")$k
  n <- attr(x, "bernstein")$n
  
  # return exp'd log function
  function(.) {
    
    out <- vector("numeric", length = length(.))

    non_pos_ndcs <- (. <= 0)
    sup_one_ndcs <- (. >= 1)
    if (any(non_pos_ndcs)) out[non_pos_ndcs] <- choose(n, k) * .[non_pos_ndcs]^k * (1 - .[non_pos_ndcs])^(n-k)
    if (any(sup_one_ndcs)) out[sup_one_ndcs] <- choose(n, k) * .[sup_one_ndcs]^k * (1 - .[sup_one_ndcs])^(n-k)
    if (!all(sup_one_ndcs | sup_one_ndcs)) out[0 < . & . < 1] <- exp(
      lchoose(n, k)  +  k*log(.[0 < . & . < 1])  +  (n-k)*log(1-.[0 < . & . < 1])
    )
    
    out
  }
  
}







# as.function.jacobi <- function(x, ...){
#   return(as.function.mpoly(x)) ## below is broken.
#   
#   # grab bernstein values
#   d <- attr(x, "jacobi")$degree
#   k <- attr(x, "jacobi")$kind
#   i <- attr(x, "jacobi")$indeterminate
#   n <- attr(x, "jacobi")$normalized
#   a <- attr(x, "jacobi")$alpha
#   b <- attr(x, "jacobi")$beta
#   
#   # return exp'd log function #
#   #http://en.wikipedia.org/wiki/Jacobi_polynomials function(.)
#   #pochhammer(a+1, d) / factorial(d) * hypergeo(-d, 1+a+b+d, a+1,
#   #(1-.)/2)
#   
# }









