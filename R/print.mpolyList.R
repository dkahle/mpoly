#' Pretty printing of a list of multivariate polynomials.
#'
#' This function iterates print.mpoly on an object of class mpolyList.
#'
#' @inheritParams print.mpoly
#' @return Invisible character vector of the printed objects.
#' @export
#' @examples
#'
#' mL <- mp(c("x + 1", "y - 1", "x y^2 z  +  x^2 z^2  +  z^2  +  x^3"))
#' mL
#' print(mL, order = "lex")
#' print(mL, order = "glex")
#' print(mL, order = "grlex")
#' print(mL, order = "glex", varorder = c("z","y","x"))
#' print(mL, order = "grlex", varorder = c("z","y","x"))
#' print(mL, varorder = c("z","y","x"))
#' 
#' (print(mL, varorder = c("z","y","x"), plus_pad = 1L, silent = TRUE))
#' 
#' (print(mL, silent = TRUE, stars = TRUE, plus_pad = 1L, times_pad = 0L))
#' 
print.mpolyList <- function(x, varorder = vars(x), stars = FALSE, order, silent = FALSE, ..., plus_pad = 2L, times_pad = 1L){
  
  stopifnot(is.mpolyList(x))
  n <- length(x)
  vars <- vars(x)
  
  if(!missing(varorder) && !all(vars %in% varorder)){
    stop(
      stri_c("If specified, varorder must contain all computed vars - ", stri_c(vars, collapse = ", ")), 
      call. = FALSE
    )
  }
  
  if(missing(varorder) && !missing(order)){
    message(stri_c('Using variable ordering - ', stri_c(vars, collapse = ', ')))
  }

  if(missing(order)){
    polys <- vapply(x, print, character(1), varorder = varorder,                stars = stars, silent = TRUE, ..., plus_pad = plus_pad, times_pad = times_pad)  
  } else {
    polys <- vapply(x, print, character(1), varorder = varorder, order = order, stars = stars, silent = TRUE, ..., plus_pad = plus_pad, times_pad = times_pad)    
  }

  if(!silent) cat(polys, sep = "\n")
  
  invisible(polys)
}








