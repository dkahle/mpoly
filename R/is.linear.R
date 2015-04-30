#' Test whether an mpoly object is linear.
#' 
#' Test whether an mpoly object is linear.
#' 
#' @param x an mpoly or mpolyList object
#' @return a logical vector
#' @export
#' @examples
#' \dontrun{
#' 
#' is.linear(mp("0"))
#' is.linear(mp("x + 1"))
#' is.linear(mp("x + y"))
#' is.linear(mp(c("0", "x + y")))
#' 
#' is.linear(mp("x + x y"))
#' is.linear(mp(c("x + x y", "x")))
#' 
#' 
#' }
is.linear <- function(x){
  
  stopifnot(is.mpoly(x) || is.mpolyList(x))
  
  if(is.mpolyList(x)) return(vapply(x, is.linear, logical(1)))

  all(
    vapply(x, function(term) all(length(term) <= 2), logical(1))
  )  
}

