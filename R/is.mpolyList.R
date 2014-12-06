#' Test whether an object is an mpolyList.
#'
#' Test whether an object is an mpolyList.
#'
#' @param mpolyList object whose class is in question
#' @return Logical.
#' @export
#' @examples
#' is.mpolyList(mp(c('x + 1', 'y - 1')))
#'
is.mpolyList <- function(mpolyList){
  if(class(mpolyList) == 'mpolyList'){
    return(TRUE)	
  } else {
    return(FALSE)	
  }
}

