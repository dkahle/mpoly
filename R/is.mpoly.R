#' Test whether an object is an mpoly object.
#'
#' Test whether an object is an mpoly object.
#'
#' @param x object to be tested
#' @return Vector of logicals.
#' @export
is.mpoly <- function(x){
  if(class(x) == 'mpoly'){
    return(TRUE)	
  } else {
    return(FALSE)	
  }
}

