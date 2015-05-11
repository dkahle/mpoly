is.number <- function(char) str_detect(char, "[0-9]{1}")
is.period <- function(char) str_detect(char, "[\\.]{1}")
is.letter <- function(char) str_detect(char, "[a-zA-Z]{1}")







#' Determine if a polynomial is a constant
#'
#' Test whether an mpoly is a constant polynomial or the elements of an mpolyList are constant polynomials.
#'
#' @param x object to be tested
#' @return Vector of logicals.
#' @export
#' @examples
#' 
#' p <- mp("5")
#' is.mpoly(p)
#' is.constant(p)
#' 
#' is.constant(mp(c("x + 1", "7", "y - 2")))
#'
is.constant <- function(p){
  if(is.mpoly(p) && length(p) == 1) return(TRUE)
  if(is.mpolyList(p)) return(vapply(p, is.constant, logical(1)))
  FALSE
}






#' Test whether an object is an mpoly object
#'
#' Test whether an object is an mpoly object.
#'
#' @param x object to be tested
#' @return Vector of logicals.
#' @export
#' @examples
#' 
#' p <- mp("x + y")
#' is.mpoly(p)
#' 
is.mpoly <- function(x) any(class(x) == "mpoly")









#' Test whether an object is an mpolyList
#'
#' Test whether an object is an mpolyList.
#'
#' @param mpolyList object whose class is in question
#' @return Logical.
#' @export
#' @examples
#' 
#' is.mpolyList(mp(c("x + 1", "y - 1")))
#'
is.mpolyList <- function(mpolyList){
  if(any(class(mpolyList) == "mpolyList")){
    return(TRUE)  
  } else {
    return(FALSE)	
  }
}









#' Test whether an mpoly object is linear
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










#' Test whether an object is a whole number
#'
#' Test whether an object is a whole number.
#'
#' @param x object to be tested
#' @param tol tolerance within which a number is said to be whole
#' @return Vector of logicals.
#' @export
#' @examples
#' is.wholenumber(seq(-3,3,.5))
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5){
  abs(x - round(x)) < tol
}

