#' Pretty printing of a list of multivariate polynomials.
#' 
#' This function iterates print.mpoly on an object of class 
#' mpolyList.
#' 
#' @param x an object of class mpolyList
#' @param varorder the order of the variables
#' @param order a total order used to order the monomials in the 
#'   printing
#' @param silent logical; if TRUE, suppresses output
#' @param ... arguments to pass to \code{\link{print.mpoly}}
#' @usage \method{print}{mpolyList}(x, varorder = vars(x), order,
#'   silent = FALSE, ...)
#' @return Invisible character vector of the printed objects.
#' @export
#' @examples
#' 
#' mL <- mp(c('x + 1', 'y - 1', 'x y^2 z  +  x^2 z^2  +  z^2  +  x^3'))
#' mL
#' 
#' ps <- print(mL, silent = TRUE)
#' ps
#' 
#' print(mL, order = 'lex')
#' print(mL, order = 'glex')
#' print(mL, order = 'grlex')
#' print(mL, order = 'glex', varorder = c('z','y','x'))
#' print(mL, order = 'grlex', varorder = c('z','y','x'))
#' print(mL, varorder = c('z','y','x'))
#' s <- print(mL, varorder = c('z','y','x'))
#' str(s)
#' 
print.mpolyList <- function(x, varorder = vars(x), order, silent = FALSE, ...){
  
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
    polys <- vapply(x, print, character(1), 
      varorder = varorder, silent = TRUE, ...)  
  } else {
    polys <- vapply(x, print, character(1), 
      varorder = varorder, order = order, silent = TRUE, ...)    
  }

  if(!silent) cat(polys, sep = "\n")
  invisible(polys)
}








