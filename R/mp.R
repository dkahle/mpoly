#' Define a multivariate polynomial.
#'
#' mp is a smart function which attempts to create a formal mpoly object from a
#' character string containing the usual representation  of a multivariate
#' polynomial.
#'
#' @param string a character string containing a polynomial, see examples
#' @param varorder (optional) order of variables in string
#' @param stars_only if you format your multiplications using asterisks, setting
#'   this to \code{TRUE} will reduce preprocessing time
#' @param vars a character vector of indeterminates
#' @return An object of class mpoly.
#' @author David Kahle \email{david@@kahle.io}
#' @seealso [mpoly()]
#' @name mp
#' @examples
#'
#' ( m <- mp("x + y + x y") )
#' is.mpoly( m )
#' unclass(m)
#' 
#' mp("1 + x")
#'
#'
#' mp("x + 2 y + x^2 y + x y z")
#' mp("x + 2 y + x^2 y + x y z", varorder = c("y", "z", "x"))
#'
#' ( ms <- mp(c("x + y", "2 x")) )
#' is.mpolyList(ms)
#'
#'
#' gradient( mp("x + 2 y + x^2 y + x y z") )
#' gradient( mp("(x + y)^10") )
#'
#' # mp and the print methods are kinds of inverses of each other
#' ( polys <- mp(c("x + y", "x - y")) )
#' strings <- print(polys, silent = TRUE)
#' strings
#' mp(strings)
#' 






#' @export
#' @rdname mp
make_indeterminate_list <- function (vars) {
  
  make_indeterminate <- function(var, ...) {
    v <- c(1, 1)
    names(v) <- c(var, "coef")
    structure(list(v), class = "mpoly")
  }

  uvars <- unique(vars)
  l <- lapply(uvars, make_indeterminate)
  names(l) <- uvars
  l
  
}
# make_indeterminate_list("a")
# make_indeterminate_list(letters) 



#' @export
#' @rdname mp
mp <- function (string, varorder, stars_only = FALSE) {
  
  # deal with mpolyLists
  if (length(string) > 1) {
    
    # do basic mpoly parsing
    ps <- structure(
      lapply(string, mp, stars_only = stars_only), 
      class = "mpolyList"
    )

    # enforce varorder if present
    if (!missing(varorder)) {
      for (k in seq_along(ps)) {
        ps[[k]] <- structure(
          lapply(ps[[k]], function(term) {
            vars_in_term <- intersect(varorder, names(term))
            term[c(vars_in_term, "coef")]
          }),
          class = "mpoly"
        )
      }
    }
    
    # return early
    return(ps)
  } 
 
  
  # clean spaces if needed
  if(!stars_only)  {
    # put *s in for spaces, twice for situations like "x y z"
    while (str_detect(string, "([\\w\\^.*\\(\\)]+) +([\\w\\^.*\\(\\)]+)")) {
      string <- str_replace_all(string, "([\\w\\^.*\\(\\)]+) +([\\w\\^.*\\(\\)]+)", "\\1*\\2")
    }
    
    # fix )('s and situations like 2(x+1)
    # string <- str_replace_all(string, pattern = "\\)\\(", replacement = ")*(")
    # string <- str_replace_all(string, "([\\w\\^.*\\(\\)]+)(\\(\\))", "\\1*\\2")
    
    # fix things like "-x"
    string <- str_replace_all(string, "^-([\\w\\^.*\\(]+)", "-1*\\1")
  }
  
  # parse using R's parser and mpoly arithmetic
  expr <- parse(text = string)[[1]]
  vars <- stringr::str_extract_all( deparse(expr),  "(?<!\\d)[a-zA-Z]\\w*" )
  vars <- unique(unlist(vars))
  p <- eval(expr, envir = make_indeterminate_list(vars))
  
  # if constant, make an mpoly
  if (is.numeric(p)) return( structure(list(c(coef = p)), class = "mpoly") )
  
  # reorder if needed and return
  if (!missing(varorder)) {
    p <- structure(
      lapply(p, function(term) {
        vars_in_term <- intersect(varorder, names(term))
        term[c(vars_in_term, "coef")]
      }),
      class = "mpoly"
    )
  }
  
  # return
  p
}

