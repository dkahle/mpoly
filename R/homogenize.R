#' Homogenize a polynomial
#' 
#' Homogenize a polynomial.
#' 
#' @param x an \code{\link{mpoly}} object
#' @param var name of homogenization
#' @return a (homogenized) mpoly
#' @name homogenize
#' @examples
#' 
#' x <- mp("x^4 + y + 2 x y^2 - 3 z")
#' is.homogeneous(x)
#' (xh <- homogenize(x))
#' is.homogeneous(xh)
#' 
#' homogenize(x, "o")
#' 
#' xh <- homogenize(x)
#' dehomogenize(xh, "t")
#' 
#' \dontrun{ demonstrates error
#' xh <- mp("x^2 t + y")
#' is.homogeneous(xh)
#' dehomogenize(xh, "t")
#' }
#' 



#' @rdname homogenize
#' @export
homogenize <- function(x, var = "t"){
  
  term_exps <- exponents(x)
  term_degs <- vapply(term_exps, sum, numeric(1))
  max_deg   <- max(term_degs)
  
  vars <- vars(x)
  p <- length(vars)
  
  homo_deg_per_term <- max_deg - term_degs

  for(k in 1:length(x)){
    if(homo_deg_per_term[[k]] == 0) next
    term <- x[[k]]
    coef_ndx <- which(names(term) == "coef")
    homo_piece <- homo_deg_per_term[[k]]
    names(homo_piece) <- var
    x[[k]] <- c(term[-coef_ndx], homo_piece, term[coef_ndx])
  }
  
  x
  
}







#' @rdname homogenize
#' @export
dehomogenize <- function(x, var){
  
  if(missing(var)) stop("dehomogenize requires a variable to dehomogenize.", call. = FALSE)
  
  # check for inhomogeneous
  if (!is.homogeneous(x)) {
    printed_mpoly <- suppressMessages(print.mpoly(x))
    stop("polynomial ", printed_mpoly, " is not homogeneous.", call. = FALSE)
  }
  
  # dehomogenize
  xdh <- lapply(x, function(term){
    var_ndx <- which(names(term) == var)
    if (length(var_ndx) == 0) {
      return(term) # var not in term  
    } else {
      return(term[-var_ndx]) 
    }
  })
  
  class(xdh) <- "mpoly"
  xdh
}




#' @rdname homogenize
#' @export
is.homogeneous <- function(x){
  term_total_degs <- vapply(exponents(x), sum, numeric(1))
  all(term_total_degs == term_total_degs[1])
}




