#' Pretty printing of multivariate polynomials.
#'
#' This is the major function used to view multivariate polynomials.
#'
#' @param x an object of class mpoly
#' @param varorder the order of the variables
#' @param order a total order used to order the monomials in the printing
#' @param stars print the multivariate polynomial in the more computer-friendly
#'   asterisk notation (default FALSE)
#' @param silent logical; if TRUE, suppresses output
#' @param ... additional parameters to go to [base::cat()]
#' @param plus_pad number of spaces to the left and right of plus sign
#' @param times_pad number of spaces to the left and right of times sign
#' @usage \method{print}{mpoly}(x, varorder, order, stars = FALSE, silent =
#'   FALSE, ..., plus_pad = 2L, times_pad = 1L)
#' @return Invisible string of the printed object.
#' @export
#' @examples
#' 
#' mp("-x^5 - 3 y^2 + x y^3 - 1")
#' 
#' 
#' (p <- mp("2 x^5  -  3 y^2  +  x y^3"))
#' print(p) # same
#' print(p, silent = TRUE)
#' s <- print(p, silent = TRUE)
#' s
#'
#' print(p, order = "lex") # -> 2 x^5  +  x y^3  -  3 y^2
#' print(p, order = "lex", varorder = c("y","x")) # -> y^3 x  -  3 y^2  +  2 x^5
#' print(p, varorder = c("y","x")) # -> y^3 x  -  3 y^2  +  2 x^5
#'
#' # this is mostly used internally
#' print(p, stars = TRUE)
#' print(p, stars = TRUE, times_pad = 0L)
#' print(p, stars = TRUE, times_pad = 0L, plus_pad = 1L)
#' print(p, stars = TRUE, times_pad = 0L, plus_pad = 0L)
#' print(p, plus_pad = 1L)
#' 
print.mpoly <- function(x, varorder, order, stars = FALSE, silent = FALSE, ..., plus_pad = 2L, times_pad = 1L) {
	
  # argument checking and basic variable setting
  stopifnot(is.mpoly(x))  
  stopifnot(is.logical(stars)) 
  vars <- vars(x) 
  
  
  # deal with constant mpolys
  if (length(vars) == 0) {
    if (!silent) cat(x[[1]], ...)
  	return(invisible(as.character(x[[1]])))
  }  
  
  
  # check variable order
  if (!missing(varorder)) {
    stopifnot(is.character(varorder))
    if(!all(vars %in% varorder)) {
      stop(
        "if specified, varorder must contain all computed vars - ", 
        stri_c(vars, collapse = ", "),
        call. = FALSE
      )
    }
  } 
  
  if (!missing(order)) match.arg(order, c("lex", "glex", "grlex"))    
  
  
  # reorder
  if (!missing(order) && !missing(varorder)) {
    x <- reorder(x, varorder = varorder, order = order)
  } else if(!missing(varorder)){
    x <- reorder(x, varorder = varorder)
  } else if(!missing(order)){
    x <- reorder(x, order = order)	
  }  
 
  
  # set delimiters
  if (!stars) { 
    
    times <- " "
    expo  <- "^"
  
  } else {
  	
    times_pad <- stringi::stri_dup(" ", times_pad)
    times <- stri_c(times_pad, "*", times_pad)
    expo  <- "**"
      	
  } 
  
  
  # now ready to start constructing the string. we don't do this by growing
  # the entire mpoly because growing in R is slow, so we print each of the terms,
  # merge them together, and take care of edge cases
  # one of the challenges here is to figure out where edge cases should be handled:
  # in the term printing or after the merging. we try to take a practical approach.
  # some things are fixed in term printing (e.g. ^1 or 1 x) and others 
  # (e.g. ?) are done in tidying the final result.
  
  
  # print terms
  terms <- vapply(x, print_term, character(1L), times = times, exp = expo) 


  # merge terms with +'s and -'s
  plus_pad <- stringi::stri_dup(" ", plus_pad)
  plus <- stri_c(plus_pad, "+", plus_pad)
  s <- stri_c(terms, collapse = plus)
  s <- stri_replace_all_fixed(s, stri_c("+", plus_pad, "-"), stri_c("-", plus_pad)) # fix subtractions 
  
  
  # final cleaning - clean "2 x  -  1 y^2"
  s <- stri_replace_all_fixed(s, stri_c("-", plus_pad, "1", times), stri_c("-", plus_pad))  
  
  
  # print
  if(!silent) cat(s, ...)
  
  
  ## return
  invisible(s)
  
}









print_term <- function(term, times = " ", expo = "^") {
  
  # if a constant term
  if (length(term) == 1L) return(format(term[["coef"]], scientific=FALSE))
  
  # split vars + exps from coef
  coef_ndx <- length(term)
  coef <- term[[ coef_ndx]]
  mono <- term [-coef_ndx]
  
  # build term
  out <- format(coef, scientific = FALSE)
  for (k in seq_along(mono)) {
    if (mono[k] == 1L) {
      out <- stri_c(out, times, names(mono)[k])
    } else {
      out <- stri_c(out, times, stri_c(names(mono)[k], expo, mono[k]))
    }  
  }

  # clean 1 from front
  if (abs(term[["coef"]] - 1) < sqrt(.Machine$double.eps) ) out <- stri_sub(out, stri_length(times) + 2L)
  
  # return
  out
  
}






