#' Multivariate polynomials in R.
#'
#' A package for symbolic computation and more with multivariate polynomials
#'
#' @importFrom stringr str_c str_detect str_sub str_replace_all str_replace
#'   str_count str_dup str_extract str_extract_all str_locate str_locate_all
#'   str_split str_sub<- str_trim fixed str_remove str_remove_all
#' @importFrom stringi stri_c stri_replace_all_fixed stri_sub stri_length
#'   stri_replace_all_regex stri_sub
#' @importFrom partitions parts
#' @importFrom plyr ddply
#' @importFrom stats deriv reorder coef
#' @importFrom ggplot2 ggplot aes
#' @importFrom polynom polynomial
#' @importFrom orthopolynom chebyshev.c.polynomials chebyshev.s.polynomials
#'   chebyshev.t.polynomials chebyshev.u.polynomials glaguerre.polynomials
#'   hermite.h.polynomials hermite.he.polynomials jacobi.g.polynomials
#'   jacobi.p.polynomials legendre.polynomials
#' @importFrom graphics contour par
#' @importFrom tidyr gather
#' @name mpoly
#' @aliases mpoly mpoly-package
#' @keywords internal
"_PACKAGE"