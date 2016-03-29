#' Polynomial extraction
#' 
#' Extract components of a multivariate polynomial
#' 
#' @param x an object of class mpoly
#' @param ndx a subsetting index
#' @param varorder the order of the variables
#' @param order a total order used to order the terms
#' @param ... additional arguments
#' @return An object of class mpoly
#' @name extract
#' @examples
#' (p <- mp("x y + x (x+1) (x+2) x z + 3 x^10"))
#' p[2]
#' p[-2]
#' p[2:3]
#' LT(p)
#' LC(p)
#' LM(p)
#' multideg(p)





#' @rdname extract
#' @export
`[.mpoly` <- function(x, ndx){
  x <- unclass(x)[ndx]
  class(x) <- "mpoly"
  x
}





#' @rdname extract
#' @export
LT <- function(x, varorder = vars(x), order = "lex"){
  reorder.mpoly(x, varorder, order)[1]
}





#' @rdname extract
#' @export
LC <- function(x, varorder = vars(x), order = "lex"){
  lt <- LT(x, varorder, order)
  unname(lt[[1]]["coef"])
}




#' @rdname extract
#' @export
LM <- function(x, varorder = vars(x), order = "lex"){
  lt <- LT(x, varorder, order)
  lt[[1]]["coef"] <- 1
  lt
}





#' @rdname extract
#' @export
multideg <- function(x, varorder = vars(x), order = "lex"){
  lt <- LT(x, varorder, order)
  coef_ndx <- which(names(lt[[1]]) == "coef")
  sparse_multideg <- lt[[1]][-coef_ndx]
  lt_vars <- names(sparse_multideg)
  nvars <- length(varorder)
  mdeg <- rep(0L, length = nvars)
  names(mdeg) <- varorder
  for (k in 1:length(lt_vars)) { 
    mdeg[lt_vars[k]] <- mdeg[lt_vars[k]] + sparse_multideg[k]
  }
  mdeg
}








