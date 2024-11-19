#' Polynomial components
#'
#' Compute quantities/expressions related to a multivariate polynomial.
#'
#' @param x an object of class mpoly
#' @param ndx a subsetting index
#' @param varorder the order of the variables
#' @param order a total order used to order the terms
#' @param reduced if TRUE, don't include zero degrees
#' @param unit for [monomials()], should the monomials have coefficient 1?
#' @param object mpoly object to pass to [coef()]
#' @param ... In [coef()], additional arguments passed to [print.mpoly()] for
#'   the names of the resulting vector
#' @param p an object of class mpoly or mpolyList
#' @param norm a norm (function) to normalize the coefficients of a polynomial, 
#'   defaults to the Euclidean norm
#' @return An object of class mpoly or mpolyList, depending on the context
#' @name components
#' @examples
#' (p <- mp("x y^2 + x (x+1) (x+2) x z + 3 x^10"))
#' p[2]
#' p[-2]
#' p[2:3]
#'
#' LT(p)
#' LC(p)
#' LM(p)
#'
#' multideg(p)
#' totaldeg(p)
#' monomials(p)
#' monomials(p, unit = TRUE)
#' coef(p)
#' 
#' p[1:2]
#' coef_lift(p[1:2])
#'
#' exponents(p)
#' exponents(p, reduce = TRUE)
#' lapply(exponents(p), is.integer)
#'
#' homogeneous_components(p)
#' 
#' (p <- mp("(x + y)^2"))
#' normalize_coefficients(p)
#' norm <- function(v) sqrt(sum(v^2))
#' norm(coef( normalize_coefficients(p) ))
#' 
#' abs_norm <- function(x) sum(abs(x))
#' normalize_coefficients(p, norm = abs_norm)
#' 
#' p <- mp(c("x", "2 y"))
#' normalize_coefficients(p)
#' 
#' # normalize_coefficients on the zero polynomial returns the zero polynomial
#' normalize_coefficients(mp("0"))
#' 
#' 





#' @rdname components
#' @export
`[.mpoly` <- function(x, ndx){
  x <- unclass(x)[ndx]
  class(x) <- "mpoly"
  x
}




#' @rdname components
#' @export
LT <- function(x, varorder = vars(x), order = "lex"){
  reorder.mpoly(x, varorder, order)[1]
}





#' @rdname components
#' @export
LC <- function(x, varorder = vars(x), order = "lex"){
  lt <- LT(x, varorder, order)
  unname(lt[[1]]["coef"])
}




#' @rdname components
#' @export
LM <- function(x, varorder = vars(x), order = "lex"){
  lt <- LT(x, varorder, order)
  lt[[1]]["coef"] <- 1
  lt
}





#' @rdname components
#' @export
multideg <- function(x, varorder = vars(x), order = "lex"){
  if(is.constant(x)) return(0L)
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





#' @rdname components
#' @export
totaldeg <- function(x){
  if(!is.mpoly(x) && length(x) > 1){
    return(vapply(x, totaldeg, integer(1)))
  }
  if(!is.mpoly(x)) stop("totaldeg requires an mpoly or mpolyList object.", call. = FALSE)
  max(vapply(exponents(x), sum, integer(1)))
}





#' @rdname components
#' @export
monomials <- function(x, unit = FALSE){
  if(!is.mpoly(x)) stop("monomials requires an mpoly or mpolyList object.", call. = FALSE)
  vec_to_mpoly <- function(.) {
    if (unit) .["coef"] <- 1
    structure(list(.), class = "mpoly")
  }
  structure(lapply(x, vec_to_mpoly), class = "mpolyList")
}






#' @rdname components
#' @export
exponents <- function(x, reduced = FALSE){

  l <- lapply(x, function(term){
    fixed_term <- as.integer(term[-which(names(term) == "coef")])
    names(fixed_term) <- names(term[-which(names(term) == "coef")])
    fixed_term
  })  
  
  if(reduced) return(l)
  
  v <- vars(x)
  p <- length(v)
  tmp <- rep.int(0L, p)
  names(tmp) <- v
  
  lapply(l, function(exp){
    tmp <- tmp + exp[v]
    tmp[is.na(tmp)] <- 0
    tmp <- as.integer(tmp)
    names(tmp) <- v
    tmp
  })
}



#' @rdname components
#' @exportS3Method 
coef.mpoly <- function(object, ...) {
  coef_vec <- vapply(object, `[[`, double(1), "coef")
  names(coef_vec) <- print.mpolyList(monomials(object, unit = TRUE), silent = TRUE, ...)
  coef_vec
}







#' @rdname components
#' @export
normalize_coefficients <- function(p, norm = function(x) sqrt(sum(x^2))) {
  if (is.mpolyList(p)) return( structure(lapply(p, normalize_coefficients), class = "mpolyList") )
  normalize <- function(x) x / norm(x)
  c <- coef(p)
  if (is.constant(p) && c == 0) return(structure(c(coef = 0), class = "mpoly"))
  c <- normalize(c)
  for (i in seq_along(p)) p[[i]]["coef"] <- c[i]
  p
}



#' @rdname components
#' @export
coef_lift <- function(p) {
  
  monos <- monomials(p, unit = TRUE)
  printed_monos <- print(monos, silent = TRUE)
  
  # remove carats and spaces, e.g. x^2 y -> x2y
  printed_monos <- gsub("\\^", "", printed_monos)
  printed_monos <- gsub(" ", "", printed_monos)
  
  # add b
  coefs_to_add <- paste0("b", printed_monos)
  
  # add coefs to mpoly
  for (i in seq_along(p)) {
    p[[i]]["coef"] <- 1
    p[[i]] <- structure(
      c(1, p[[i]]),
      names = c(coefs_to_add[i], names(p[[i]]))
    )
  }
  
  # return p
  p

}






