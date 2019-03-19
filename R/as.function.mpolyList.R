#' Change a vector of multivariate polynomials into a function.
#'
#' Transforms an mpolyList object into a function which can be evaluated.
#'
#' @inheritParams as.function.mpoly
#' @usage \method{as.function}{mpolyList}(x, varorder = vars(x), vector = TRUE,
#'   silent = FALSE, ..., plus_pad = 1L, times_pad = 1L, squeeze = TRUE)
#' @seealso [plug()], [as.function.mpolyList()]
#' @export
#' @examples
#'
#' # basic examples
#' (mpolyList <- mp(c("2 x + 1", "x - z^2")))
#' (f <- as.function(mpolyList))
#' f(c(1,2)) # -> (2*1 + 1, 1-2^2) = 3 -3
#'
#' f <- as.function(mpolyList, varorder = c("x","y","z"))
#' f(c(1,0,2)) # -> 3 -3
#' f(c(1,4,2)) # -> 3 -3
#'
#' f <- as.function(mpolyList, varorder = c("x","y","z"), vector = FALSE)
#' f(1, 0, 2) # -> 3 -3
#' f(1, 4, 2) # -> 3 -3
#'
#'
#'
#' # making a gradient function (useful for optim)
#' mpoly <- mp("x + y^2 + y z")
#' mpolyList <- gradient(mpoly)
#' f <- as.function(mpolyList, varorder = vars(mpoly))
#' f(c(0,2,3)) # -> 1 7 2
#'
#'
#'
#' # a univariate mpolyList creates a vectorized function
#' ps <- mp(c("x", "x^2", "x^3"))
#' f <- as.function(ps)
#' f
#' s <- seq(-1, 1, length.out = 11)
#' f(s)
#'
#' # another example
#' ps <- chebyshev(1:3)
#' f <- as.function(ps)
#' f(s)
#'
#' # the binomial pmf as an algebraic (polynomial) map
#' # from [0,1] to [0,1]^size
#' # p |-> {choose(size, x) p^x (1-p)^(size-x)}_{x = 0, ..., size}
#' abinom <- function(size, indet = "p"){
#'   chars4mp <- vapply(as.list(0:size), function(x){
#'     sprintf("%d %s^%d (1-%s)^%d", choose(size, x), indet, x, indet, size-x)
#'   }, character(1))
#'   mp(chars4mp)
#' }
#' (ps <- abinom(2, "p")) # = mp(c("(1-p)^2", "2 p (1-p)", "p^2"))
#' f <- as.function(ps)
#'
#' f(.5) # P[X = 0], P[X = 1], and P[X = 2] for X ~ Bin(2, .5)
#' dbinom(0:2, 2, .5)
#'
#' f(.75) # P[X = 0], P[X = 1], and P[X = 2] for X ~ Bin(2, .75)
#' dbinom(0:2, 2, .75)
#'
#' # as the degree gets larger, you'll need to be careful when evaluating
#' # the polynomial.  as.function() is not currently optimized for
#' # stable numerical evaluation of polynomials; it evaluates them in
#' # the naive way
#' all.equal(
#'   as.function(abinom(10))(.5),
#'   dbinom(0:10, 10, .5)
#' )
#'
#' all.equal(
#'   as.function(abinom(30))(.5),
#'   dbinom(0:30, 20, .5)
#' )
#'
#'
#' # the function produced is vectorized:
#' number_of_probs <- 11
#' probs <- seq(0, 1, length.out = number_of_probs)
#' (mat <- f(probs))
#' colnames(mat) <- sprintf("P[X = %d]", 0:2)
#' rownames(mat) <- sprintf("p = %.2f", s)
#' mat
#' 
as.function.mpolyList <- function(x, varorder = vars(x), vector = TRUE, silent = FALSE, ..., plus_pad = 1L, times_pad = 1L, squeeze = TRUE){
  
  # argument checking
  stopifnot(is.character(varorder))
  stopifnot(is.logical(vector))  	
  stopifnot(is.mpolyList(x))

  if (!missing(varorder) && !all( vars(x) %in% varorder )) stop("varorder must contain all of the variables.", call. = FALSE)
  
  
  # determine the number of variables
  p <- length(varorder)
    
  
  # print polys with stars
  mp_str <- print.mpolyList(x, stars = TRUE, silent = TRUE, plus_pad = plus_pad, times_pad = times_pad)
  

  # univariate polynomial - vectorize
  if (p == 1) {
    mp_str <- stri_c(mp_str, collapse = " , ")
    mp_str <- stri_replace_all_fixed(mp_str, varorder, ".")
    if (squeeze) mp_str <- stri_replace_all_fixed(mp_str, " ", "") 
    f <- function(){}
    formals(f) <- alist(. = )
    body(f) <- as.call(c(
      as.name("{"),
      expression(if(length(.) > 1) return(t(sapply(., f)))),
      parse(text = stri_c("c(", mp_str, ")"))
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
    if((silent == FALSE) && (missing(vector) || missing(varorder))) message("f(.) with . = ", v)
    mp_str <- stri_c("function(.){ c(", stri_c(mp_str, collapse = ", "), ") }")    
    return(eval(parse(text = mp_str)))
  }
  
  # general polynomials as a bunch of arguments
  if (!vector) {
    if (squeeze) mp_str <- stri_replace_all_fixed(mp_str, " ", "") 
    mp_str <- stri_c(mp_str, collapse = ", ")
    if((silent == FALSE) && (missing(vector) || missing(varorder))) message("f(", paste(varorder, collapse = ", "), ")", sep = "")
    mp_str <- stri_c(
      "function(", stri_c(varorder, collapse = ", "), ") { c(", 
      mp_str, 
      ") }"
    )
    return(eval(parse(text = mp_str)))
  }  
  
  
  
}





















































