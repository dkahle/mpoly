#' Change polynomials into functions.
#'
#' Transform \code{mpoly} and \code{mpolyList} objects into function that can be
#' evaluated.
#'
#' Convert polynomials to functions mapping Rn to Rm that are vectorized in the
#' following way [as.function.mpoly()] governs this behavior:
#' 
#' \describe{
#'
#' \item{\[m = 1, n = 1, e.g. f(x) = x^2 + 2x\] }{Ordinary vectorized function
#' returned, so that if you input a numeric vector x, the returned function is
#' evaluated at each of the input values, and a numeric vector is returned.}
#'
#' \item{\[m = 1, n = 2+, e.g. f(x,y) = x^2 + 2x + 2xy\] }{A function of a
#' single vector argument f(v) = f(c(x,y)) is returned. If a N x n matrix is
#' input to the returned function, the function will be applied across the rows
#' and return a numeric vector of length N. If desired, setting `vector = FALSE`
#' changes this behavior so that an arity-n function is returned, i.e. the
#' function f(x,y) of two arguments.} In this case, the returned function will
#' accept equal-length numeric vectors and return a numeric vector, vectorizing
#' it.
#' 
#' }
#' 
#' And [as.function.mpolyList()] governs this behavior:
#' 
#' \describe{
#'
#' \item{\[m = 2+, n = 1, e.g. f(x) = (x, x^2)\] }{Ordinary vectorized function
#' returned, so that if you input a numeric vector x, the function is evaluated
#' at each of the input values, and a numeric matrix N x m, where N is the
#' length of the input vector.}
#' 
#' \item{\[m = 2+, n = 2+, e.g. f(x,y) = (1, x, y)\] }{When `vector = FALSE`
#' (the default), the created function accepts a numeric vector and returns a
#' numeric vector. The function will also accept an N x n matrix, in which case
#' the function is applied to its rows to return a N x m matrix.}
#'
#' }
#'
#' @inheritParams print.mpoly
#' @param vector whether the function should take a vector argument (TRUE) or a
#'   series of arguments (FALSE)
#' @param name should the returned object be named? only for `mpolyList` objects
#' @param ... any additional arguments
#' @param squeeze minify code in the created function
#' @name as-function
#' @seealso [plug()]
#' @examples
#' 
#' # basic usage. m = # polys/eqns, n = # vars
#' 
#' # m = 1, n = 1, `as.function.mpoly()`
#' p <- mp("x^2 + 1")
#' (f <- as.function(p))
#' f(2)
#' f(1:3) # vectorized
#' 
#' 
#' # m = 1, n = 2 , `as.function.mpoly()`
#' p <- mp("x y")
#' (f <- as.function(p))
#' f(1:2) 
#' (mat <- matrix(1:6, ncol = 2))
#' f(mat) # vectorized across rows of input matrix
#' 
#'  
#' # m = 2, n = 1, `as.function.mpolyList()`
#' p <- mp(c("x", "x^2"))
#' (f <- as.function(p))
#' f(2)
#' f(1:3) # vectorized
#' 
#' (f <- as.function(p, name = TRUE))
#' f(2)
#' f(1:3) # vectorized
#' 
#' 
#' # m = 3, n = 2, `as.function.mpolyList()`
#' p <- mp("(x + y)^2")
#' (p <- monomials(p))
#' 
#' (f <- as.function(p))
#' f(1:2) 
#' (mat <- cbind(x = 1:3, y = 4:6))
#' f(mat) # vectorized across rows of input matrix
#' 
#' (f <- as.function(p, name = TRUE))
#' f(1:2) 
#' f(mat)
#' 
#' 
#'
#' # setting vector = FALSE changes the function to a sequence of arguments
#' # this is only of interest if n = # of vars > 1
#' 
#' # m = 1, n = 2, `as.function.mpoly()`
#' p <- mp("x y")
#' (f <- as.function(p, vector = FALSE))
#' f(1, 2) 
#' (mat <- matrix(1:6, ncol = 2))
#' f(mat[,1], mat[,2]) # vectorized across input vectors
#' 
#' # m = 3, n = 2, `as.function.mpolyList()`
#' p <- mp(c("x", "y", "x y"))
#' (f <- as.function(p, vector = FALSE))
#' f(1, 2) 
#' (mat <- matrix(1:4, ncol = 2))
#' f(mat[,1], mat[,2]) # vectorized across rows of input matrix
#' (f <- as.function(p, vector = FALSE, name = TRUE))
#' f(1, 2) 
#' (mat <- matrix(1:4, ncol = 2))
#' f(mat[,1], mat[,2]) # vectorized across rows of input matrix
#' 
#' 
#' 
#' # it's almost always a good idea to use the varorder argument, 
#' # otherwise, mpoly has to guess at the order of the arguments
#' invisible( as.function(mp("y + x")) )
#' invisible( as.function(mp("x + y")) )
#' invisible( as.function(mp("y + x"), varorder = c("x","y")) )
#'
#'
#' # constant polynomials have some special rules
#' f <- as.function(mp("1"))
#' f(2)
#' f(1:10)
#' f(matrix(1:6, nrow = 2))
#' 
#'
#'
#' # you can use this to create a gradient function, useful for optim()
#' p <- mp("x + y^2 + y z")
#' (ps <- gradient(p))
#' (f <- as.function(ps, varorder = c("x","y","z")))
#' f(c(0,2,3)) # -> [1, 7, 2]
#'
#'
#'
#' # a m = 1, n = 2+ mpolyList creates a vectorized function
#' # whose rows are the evaluated quantities
#' (ps <- basis_monomials("x", 3))
#' (f <- as.function(ps))
#' s <- seq(-1, 1, length.out = 11)
#' f(s)
#'
#' # another example
#' (ps <- chebyshev(1:3))
#' f <- as.function(ps)
#' f(s)
#'
#' # the binomial pmf as an algebraic (polynomial) map
#' # from [0,1] to [0,1]^size
#' # p |-> {choose(size, x) p^x (1-p)^(size-x)}_{x = 0, ..., size}
#' abinom <- function(size, indet = "p"){
#'   chars4mp <- vapply(0:size, function(x){
#'     sprintf("%d %s^%d (1-%s)^%d", choose(size, x), indet, x, indet, size-x)
#'   }, character(1))
#'   mp(chars4mp)
#' }
#' (ps <- abinom(2, "p")) # = mp(c("(1-p)^2", "2 p (1-p)", "p^2"))
#' f <- as.function(ps)
#'
#' f(.50) # P[X = 0], P[X = 1], and P[X = 2] for X ~ Bin(2, .5)
#' dbinom(0:2, 2, .5)
#'
#' f(.75) # P[X = 0], P[X = 1], and P[X = 2] for X ~ Bin(2, .75)
#' dbinom(0:2, 2, .75)
#'
#' f(c(.50, .75)) # the above two as rows
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
#' rownames(mat) <- sprintf("p = %.2f", probs)
#' mat
#' 









#' @usage \method{as.function}{mpoly}(x, varorder = vars(x), vector = TRUE,
#'   silent = FALSE, ..., plus_pad = 1L, times_pad = 1L, squeeze = TRUE)
#' @rdname as-function
#' @exportS3Method base::as.function
as.function.mpoly <- function(x, varorder = vars(x), vector = TRUE, silent = FALSE, ..., plus_pad = 1L, times_pad = 1L, squeeze = TRUE){
  
  # argument checking
  stopifnot(is.character(varorder))
  stopifnot(is.logical(vector))  	
  stopifnot(is.mpoly(x))
  if (!setequal(varorder, vars(x))) {
    stop("`varorder` must contain all of the input polynomial.", call. = FALSE)
  }
  
  
  # determine the number of variables
  n <- length(vars(x))
  
  
  # deal with constant polynomials
  if (is.constant(x)) return( function(.) 0*. + unlist(x)[["coef"]] )
  
  
  # print poly with stars
  mp_str <- print.mpoly(x, stars = TRUE, silent = TRUE, plus_pad = plus_pad, times_pad = times_pad)
  
  
  # univariate polynomial
  if (n == 1L) {
    mp_str <- stri_replace_all_fixed(mp_str, vars(x), ".")
    mp_str <- stri_replace_all_fixed(mp_str, "**", " ^")
    if (squeeze) mp_str <- stri_replace_all_fixed(mp_str, " ", "")
    if (!silent) message("f(.) with . = ", vars(x))
    return(eval(parse(text = paste0("function(.) ", mp_str))))
  }
  
  
  # general polynomials as a vector argument
  if (vector) { 
    mp_str <- stri_c(" ", mp_str, " ") # pad to make parsing easier 
    for (k in 1L:n) {
      mp_str <- stri_replace_all_fixed(mp_str, "**", " ^")
      mp_str <- stri_replace_all_fixed(mp_str, stri_c(" ", varorder[k], " "), stri_c(" .[", k, "] "))
      mp_str <- stri_replace_all_fixed(mp_str, " ^", "**")
    } 
    if (squeeze) mp_str <- stri_replace_all_fixed(mp_str, " ", "") 
    v <- stri_c("(", stri_c(varorder, collapse = ", "), ")") 
    if(!silent) message("f(.) with . = ", v) 
    f <- function(){}
    formals(f) <- alist(. = )
    body(f) <- as.call(c(
      as.name("{"),
      expression(if(is.matrix(.)) {
        if (ncol(.) != n) 
          stop("`ncol(mat)` not equal to number of variables.", call. = FALSE)
        return(apply(., 1, f))
      }),
      parse(text = mp_str)
    ))
    return(f)
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








#' @rdname as-function
#' @exportS3Method base::as.function
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
#   #https://en.wikipedia.org/wiki/Jacobi_polynomials function(.)
#   #pochhammer(a+1, d) / factorial(d) * hypergeo(-d, 1+a+b+d, a+1,
#   #(1-.)/2)
#   
# }











#' @usage \method{as.function}{mpolyList}(x, varorder = vars(x), vector = TRUE,
#'   silent = FALSE, name = FALSE, ..., plus_pad = 1L, times_pad = 1L, squeeze = TRUE)
#' @rdname as-function
#' @exportS3Method base::as.function
as.function.mpolyList <- function(x, varorder = vars(x), vector = TRUE, silent = FALSE, name = FALSE, ..., plus_pad = 1L, times_pad = 1L, squeeze = TRUE){
  
  # argument checking
  stopifnot(is.character(varorder))
  stopifnot(is.logical(vector))  	
  stopifnot(is.mpolyList(x))

  if (!missing(varorder) && !all( vars(x) %in% varorder )) stop("varorder must contain all of the variables.", call. = FALSE)
  
  
  # determine the number of variables
  n <- length(varorder)
    
  
  # print polys with stars
  mp_str <- print.mpolyList(x, stars = TRUE, silent = TRUE, plus_pad = plus_pad, times_pad = times_pad)
  if (name) printed_mps <- print.mpolyList(x, silent = TRUE)
  

  # univariate polynomial - vectorize
  if (n == 1) {
    mp_str <- stri_c(mp_str, collapse = " , ")
    mp_str <- stri_replace_all_fixed(mp_str, varorder, ".")
    if (squeeze) mp_str <- stri_replace_all_fixed(mp_str, " ", "") 
    f <- function(){}
    formals(f) <- alist(. = )
    body(f) <- as.call(c(
      as.name("{"),
      expression(if(length(.) > 1) return(t(sapply(., f)))),
      parse(text = paste0("out <- c(", stri_c(mp_str, collapse = ", "), ")")),
      expression(if (name) structure(out, names = printed_mps) else out)
    ))
    return(f)
  }
  
  # general polynomials as a vector argument
  if (vector) {
    mp_str <- stri_c(" ", mp_str, " ") # pad to make parsing easier 
    for (k in 1L:n) {
      mp_str <- stri_replace_all_fixed(mp_str, "**", " ^")
      mp_str <- stri_replace_all_fixed(mp_str, stri_c(" ", varorder[k], " "), stri_c(" .[", k, "] "))
      mp_str <- stri_replace_all_fixed(mp_str, " ^", "**")
    } 
    if (squeeze) mp_str <- stri_replace_all_fixed(mp_str, " ", "") 
    v <- stri_c("(", stri_c(varorder, collapse = ", "), ")")
    if(!silent && (missing(vector) || missing(varorder))) message("f(.) with . = ", v)
    f <- function(){}
    formals(f) <- alist(. = )
    body(f) <- as.call(c(
      as.name("{"),
      expression(if (!name) . <- unname(.)),
      expression(if(is.matrix(.)) {
        if (ncol(.) != n) 
          stop("`ncol(mat)` not equal to number of variables.", call. = FALSE)
        return(t(apply(., 1, f)))
      }),
      parse(text = paste0("out <- c(", stri_c(mp_str, collapse = ", "), ")")),
      expression(if (name) structure(out, names = printed_mps) else out)
    ))
    return(f)
    
  }

  # general polynomials as a bunch of arguments
  if (!vector) {
    if (squeeze) mp_str <- stri_replace_all_fixed(mp_str, " ", "") 
    mp_str <- stri_c(mp_str, collapse = ", ")
    if(!silent && (missing(vector) || missing(varorder))) {
      message("f(", paste(varorder, collapse = ", "), ")", sep = "")
    }
    mp_str <- stri_c(
      "function(", stri_c(varorder, collapse = ", "), ") {", 
      "if (length(", varorder[1], ") > 1) {",
      "out <- unname(cbind(", mp_str, ")); return(if (name) structure(out, dimnames = list(NULL, printed_mps)) else out) ",
      "};", 
      # "c(", mp_str, ")",
      paste0("out <- c(", mp_str, ");"),
      "if (name) structure(out, names = printed_mps) else out",
      "}"
    )
    return(eval(parse(text = mp_str)))
  }  
  
}



