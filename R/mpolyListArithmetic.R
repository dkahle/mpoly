#' Element-wise arithmetic with vectors of multivariate polynomials.
#'
#' Element-wise arithmetic with vectors of multivariate polynomials.
#'
#' @param e1 an object of class mpolyList
#' @param e2 an object of class mpolyList
#' @return An object of class mpolyList.
#' @name mpolyListArithmetic
#' @examples
#'
#' ( ms1 <- mp( c("x", 'y') ) )
#' ( ms2 <- mp( c("y", '2 x^2') ) )
#' ms1 + ms2
#' ms1 - ms2
#' ms1 * ms2
#' ms1^3
#' 
NULL







#' @rdname mpolyListArithmetic
#' @export
`+.mpolyList` <- function(e1, e2){
  
  # argument check
  if (is.numeric(e1)) {
    e1 <- structure(
      lapply(e1, function(.) structure(list(c("coef" = .)), class = "mpoly")),
      class = "mpolyList"
    )
  }
  # if (is.mpoly(e1)) e1 <- mpolyList(e1)
  
  if (is.numeric(e2)) {
    e2 <- structure(
      lapply(e2, function(.) structure(list(c("coef" = .)), class = "mpoly")),
      class = "mpolyList"
    )
  }
  # if (is.mpoly(e2)) e2 <- mpolyList(e2)
  
  stopifnot(is.mpolyList(e1))
  stopifnot(is.mpolyList(e2))
  
  # fix lengths
  if (length(e1) == 1L && length(e2) != 1L) {
    e1 <- structure(
      replicate(length(e2), e1[[1L]], simplify = FALSE),
      class = "mpolyList"
    )
  }
  
  if (length(e1) != 1L && length(e2) == 1L) {
    e2 <- structure(
      replicate(length(e1), e2[[1L]], simplify = FALSE),
      class = "mpolyList"
    )
  }
  
  if(length(e1) != length(e2)) stop("e1 and e2 must have equal length.", call. = FALSE)

  
  # template outcome
  out <- vector(length = length(e1), mode = "list")
  
  # compute sums
  for (k in seq_along(out)) out[[k]] <- e1[[k]] + e2[[k]]
  
  # return
  structure(out, class = "mpolyList")
}	












#' @rdname mpolyListArithmetic
#' @export
`-.mpolyList` <- function(e1, e2) e1 + -1*e2











#' @rdname mpolyListArithmetic
#' @export
`*.mpolyList` <- function(e1, e2){
  
  # argument check
  if (is.numeric(e1)) {
    e1 <- structure(
      lapply(e1, function(.) structure(list(c("coef" = .)), class = "mpoly")),
      class = "mpolyList"
    )
  }
  # if (is.mpoly(e1)) e1 <- mpolyList(e1)
  
  if (is.numeric(e2)) {
    e2 <- structure(
      lapply(e2, function(.) structure(list(c("coef" = .)), class = "mpoly")),
      class = "mpolyList"
    )
  }
  # if (is.mpoly(e2)) e2 <- mpolyList(e2)
  
  stopifnot(is.mpolyList(e1))
  stopifnot(is.mpolyList(e2))
  
  # fix lengths
  if (length(e1) == 1L && length(e2) != 1L) {
    e1 <- structure(
      replicate(length(e2), e1[[1L]], simplify = FALSE),
      class = "mpolyList"
    )
  }
  
  if (length(e1) != 1L && length(e2) == 1L) {
    e2 <- structure(
      replicate(length(e1), e2[[1L]], simplify = FALSE),
      class = "mpolyList"
    )
  }
  
  if(length(e1) != length(e2)) stop("e1 and e2 must have equal length.", call. = FALSE)
  
  # template outcome
  out <- vector(length = length(e1), mode = "list")
  
  # compute products
  for (k in seq_along(out)) out[[k]] <- e1[[k]] * e2[[k]]
  
  # return
  structure(out, class = "mpolyList")
}	









#' @rdname mpolyArithmetic
#' @export
`^.mpolyList` <- function(e1, e2){
  
  stopifnot(is.mpolyList(e1))
  
  if(!is.wholenumber(e2) || e2 < 0) stop("exponent must be a positive integer.", call. = FALSE)
  
  out <- structure(
    replicate(
      length(e1),
      structure(list(c("coef" = 1L)), class = "mpoly"),
      simplify = FALSE
    ),
    class = "mpolyList"
  )
  
  if(e2 == 0) return(out)
  
  for(k in 1:e2) out <- out * e1
  
  out
}

