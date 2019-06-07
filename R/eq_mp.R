#' Convert an equation to a polynomial
#'
#' Convert characters of the form \code{"p1 = p2"} (or similar) to an mpoly
#' object representing \code{p1 - p2}.
#'
#' @param string a character string containing a polynomial, see examples
#' @param ... arguments to pass to [mpoly()]
#' @return An object of class mpoly or mpolyList.
#' @author David Kahle \email{david@@kahle.io}
#' @seealso [mpoly()]
#' @export
#' @examples
#'
#' eq_mp(c("y = x", "y ==  (x + 2)"))
#'
#'
#' 

eq_mp <- function (string, ...) {
  
  # "y = x" -> "(y) - (x)"
  strings <- vapply(
    stringi::stri_split_regex(string, "\\s*==?\\s*"),
    function(x) stringi::stri_c("(", x[1], ") - (", x[2], ")"),
    character(1)
  )
  
  # parse and output
  mp(strings, ...)
  
}


