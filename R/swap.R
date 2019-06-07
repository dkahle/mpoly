#' Swap polynomial indeterminates
#'
#' Swap polynomial indeterminates
#'
#' @param p polynomial
#' @param variable the variable in the polynomial to replace
#' @param replacement the replacement variable
#' @return a mpoly object
#' @author David Kahle
#' @export
#' @examples
#'
#' (p <- mp("(x + y)^2"))
#' swap(p, "x", "t")
#'
#' ## the meta data is retained
#' (p <- bernstein(3, 5))
#' (p2 <- swap(p, "x", "t"))
#' is.bernstein(p2)
#'
#' (p <- chebyshev(3))
#' (p2 <- swap(p, "x", "t"))
#' is.chebyshev(p2)
#'
#' 
swap <- function(p, variable, replacement){


  # arg check
  stopifnot(is.mpoly(p))
  if (stringi::stri_detect_regex(replacement, "[^\\w.]")) stop("replacement must be a single symbol.", call. = FALSE)

  # determine variables
  vars <- vars(p)

  # if constant, return
  if(is.constant(p)) return(p)
  

  # don't allow replacement of one variable in the presence of many when
  # the variable is already a part of the polynomial, since
  # mpoly won't be run again to determine if a variable is duplicated
  stopifnot(variable %in% vars)
  if(replacement %in% vars && length(vars) > 1){
    stop("the replacement value cannot be a variable in the polynomial, try plug.", call. = FALSE)
  }
  
  
  # swapping in polynomial
  p_out <- unclass(p)
  p_out <- lapply(p, function(v) {
    names(v)[names(v) == variable] <- replacement
    v
  })
  class(p_out) <- "mpoly"
  
  # custom for special polynomials
  if (is.bernstein(p)) {
    class(p_out) <- c("bernstein", "mpoly")
    attr(p_out, "bernstein") <- attr(p, "bernstein")
    attr(p_out, "bernstein")["indeterminate"] <- replacement
  }
  
  if (is.chebyshev(p)) {
    class(p_out) <- c("chebyshev", "mpoly")
    attr(p_out, "chebyshev") <- attr(p, "chebyshev")
    attr(p_out, "chebyshev")["indeterminate"] <- replacement
  }
  
  # return
  p_out
}



