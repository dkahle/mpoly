#' Pretty printing of a list of multivariate polynomials.
#' 
#' This function iterates print.mpoly on an object of class 
#' mpolyList.
#' 
#' @inheritParams print.mpoly
#' @return Invisible character vector of the printed objects.
#' @export
#' @examples
#' 
#' mL <- mp(c("x + 1", "y - 1", "x y^2 z  +  x^2 z^2  +  z^2  +  x^3"))
#' mL
#' 
#' ps <- print(mL, silent = TRUE, plus_pad = 1L)
#' ps
#' 
#' print(mL, order = "lex")
#' print(mL, order = "glex")
#' print(mL, order = "grlex")
#' print(mL, order = "glex", varorder = c("z","y","x"))
#' print(mL, order = "grlex", varorder = c("z","y","x"))
#' print(mL, varorder = c("z","y","x"))
#' s <- print(mL, varorder = c("z","y","x"), plus_pad = 1L)
#' str(s)
#' 
print.mpolyList <- function(x, varorder = vars(x), order, silent = FALSE, ..., plus_pad = 2L, times_pad = 0L){
  
  stopifnot(is.mpolyList(x))
  n <- length(x)
  vars <- vars(x)
  
  if(!missing(varorder) && !all(vars %in% varorder)){
    error <- paste("if specified, varorder must contain all computed vars - ", 
        paste(vars, collapse = ", "), sep = "")
    stop(error, call. = FALSE)
  }
  
  if(missing(varorder) && !missing(order)){
    message <- paste(
      'using variable ordering - ',
      paste(vars, collapse = ', '),
      sep = ''
    )
    message(message)
  }

  if(missing(order)){
    polys <- vapply(x, print, character(1), varorder = varorder, silent = TRUE, ..., plus_pad = plus_pad, times_pad = times_pad)  
  } else {
    polys <- vapply(x, print, character(1), varorder = varorder, order = order, silent = TRUE, ..., plus_pad = plus_pad, times_pad = times_pad)    
  }

  if(!silent) cat(polys, sep = "\n")
  invisible(polys)
}








