#' Define a multivariate polynomial.
#'
#' mp is a smart function which attempts to create a formal mpoly object from a
#' character string containing the usual representation  of a multivariate
#' polynomial.
#'
#' @param string a character string containing a polynomial, see examples
#' @param varorder (optional) order of variables in string
#' @param stars_only only useful after version 2.0; if you format your
#'   multiplications using asterisks, setting this to \code{TRUE} will reduce
#'   preprocessing time
#' @param vars a character vector of indeterminates
#' @return An object of class mpoly.
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @seealso \code{\link{mpoly}}
#' @name mp
#' @examples
#' 
#' ( m <- mp("x + y + x y") )
#' is.mpoly( m )
#' unclass(m)
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
    return(structure(lapply(string, mp), class = "mpolyList"))
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
  if (!missing(varorder)) p <- reorder.mpoly(p)
  p
  
}



# 
# 
# 
# mp <- function(string, varorder){
# 
#   stopifnot(is.character(string))
# 
#   # if string is a vector of polys, return mpolyList
#   if(length(string) > 1){
#     if(missing(varorder)){
#       mpolyList <- lapply(string, mp)
#     } else {
#       mpolyList <- lapply(string, mp, varorder = varorder)
#     }
#     class(mpolyList) <- "mpolyList"
#     return(mpolyList)
#   }
# 
#   # switch *'s for " "'s
#   string <- str_replace_all(string, fixed("*"), " ")
# 
#   # fix leading -
#   string <- str_replace(string, "^(\\s*-)", "0 + -1 ")
# 
#   # clean whitespace
#   string <- str_replace_all(string, " {2,}", " ")
#   string <- str_replace_all(string, "[\\n\\t]", " ")
# 
#   # clean -(x + y) -> -1 (x + y)
#   string <- str_replace_all(string, "^ *- *\\(", "-1 (")
# 
#   # clean double parens
#   string <- str_replace_all(string, fixed(")("), ") (")
# 
#   # check for bad things
#   unmatched_parentheses_stop(string)
#   empty_parenthetical_stop(string)
# 
#   # compute
#   out <- parse_parenthetical_polynomial(string)
# 
#   # check varorder argument
#   if(!missing(varorder)){
# 
#     vars <- vars(out)
# 
#     if(!all(vars %in% varorder)){
#       error <- sprintf(
#         "If specified, varorder must contain all computed vars - %s",
#         str_c(vars, collapse = ", ")
#       )
#       stop(error, call. = FALSE)
#     }
# 
#     # order vars appropriately
#     vars <- intersect(varorder, vars)
#     out  <- reorder.mpoly(out, varorder = vars)
#   }
# 
#   # return
#   out
# 
# }
# 
# 
# 





# string <- "x ((x+y)+2)"
# parse_parenthetical_polynomial(string)
# parse_parenthetical_polynomial("x ((x+y) + 2)")
# parse_parenthetical_polynomial("-(x + y) + 2 x (x + y)^2 + 3 y")
parse_parenthetical_polynomial <- function(string){
  
  # fix term joins
  terms <- extract_polynomial_terms(string)
  
  # parse into mpolys
  mpolys <- lapply(terms, parse_parenthetical_term)
  
  # add and return
  Reduce(`+.mpoly`, mpolys)
  
}



























# parse_parenthetical_term("3 y")
# parse_parenthetical_term(" 3 (x + y) 4 (x - y) ")
# parse_parenthetical_term("(x + y) (x - y)")
# parse_parenthetical_term("(x + y)")
# parse_parenthetical_term("(x + y)^2")
# parse_parenthetical_term("(x + y)(x-y)")
# parse_parenthetical_term("-2 (x + y)^2 (x - y)^0 4 (1+1)^3")
# 
# # more complex usage
# parse_parenthetical_term("((x^2))") 
# parse_parenthetical_term("((5^2))") 
# string <- "(1+1) (2^3 z (x+y)^2)^2"
# parse_parenthetical_term(string)
# parse_parenthetical_term("6 (x)")
# parse_parenthetical_term("6.18033988749895 (x)")
parse_parenthetical_term <- function(string){
  
  # short circuit if simpler
  if(!contains_parenthetical_expression(string)) 
    return(parse_nonparenthetical_term(string))
  
  # break into parenthetical pieces ("bubbles")
  pieces <- term_parentheticals(string)
  pieces <- pieces[pieces != ""]
  
  # mpoly pieces
  mpolys <- lapply(pieces, function(piece){
    
    # identify expression and exponent components
    expr <- str_extract(piece, "\\([-()\\w+ \\^.]+\\)")
    expr <- str_sub(expr, 2, -2) # take off parens
    
    # check for exponent on the outer parenthetical
    last_paren_ndx     <- nchar(piece) - str_locate(str_rev(piece), fixed(")"))[[1]] + 1L
    string_after_paren <- str_sub(piece, last_paren_ndx + 1L) # "" or "^3"
    
    # if "^3", extract, otherwise 1
    if(str_detect(string_after_paren, fixed("^"))){
      exponent <- as.numeric(str_rev(str_extract(str_rev(string_after_paren), "[0-9]+"))) # gets first
    } else {
      exponent <- 1
    }
    
    # parse
    if(contains_nested_parenthetical_expression(piece)){
      parse_parenthetical_polynomial(expr)^exponent  
    } else {
      parse_nonparenthetical_polynomial(expr)^exponent
    }
    
  })
  
  # product and return
  Reduce(`*.mpoly`, mpolys)
  
}











































# parse_nonparenthetical_polynomial(" -1")
# parse_nonparenthetical_polynomial("x-1")
# parse_nonparenthetical_polynomial("5-2x")
# parse_nonparenthetical_polynomial("5 - 2     x")
# parse_nonparenthetical_polynomial("5 + -2x")
# parse_nonparenthetical_polynomial("1--1")
# parse_nonparenthetical_polynomial("1 - - 1")
# parse_nonparenthetical_polynomial("5^2x")
# parse_nonparenthetical_polynomial("5^2-x")
# parse_nonparenthetical_polynomial("-x")
# parse_nonparenthetical_polynomial("-1")
# parse_nonparenthetical_polynomial("1+-x-x")
# parse_nonparenthetical_polynomial("1 - -3")
#
# parse_nonparenthetical_polynomial("-x + 2y - 4x - -4")
#
# string <- "-4 + 2+2 x +   1 x y^4 -3 prq^3 -y - 3 x 2 - 3 y -2"
# parse_nonparenthetical_polynomial(string)
# parse_nonparenthetical_polynomial("x    +       y")
# parse_nonparenthetical_polynomial("x    -       y+-xy")
# parse_nonparenthetical_polynomial("1e-2 x")
# parse_nonparenthetical_polynomial("1e+2 x")
parse_nonparenthetical_polynomial <- function(string){
  
  # check to see if it's a single term
  if (!str_detect(string, "[+]") && !str_detect(str_sub(string, 2), "[-]")) {
    return(parse_nonparenthetical_term(string))
  }
  
  # regularize term joins (deal with minuses)
  string <- fix_term_joins(string)  
  
  # split polynomial
  terms <- str_split(string, fixed(" + "))[[1]]
  
  # parse terms
  mpolyTerms <- lapply(terms, parse_nonparenthetical_term)
  
  # combine and return
  Reduce(`+.mpoly`, mpolyTerms)
  
}








# parse_parenthetical_term("t1a")
# parse_nonparenthetical_term("12var 2 y 2x")
# parse_nonparenthetical_term("-2      7")
# parse_nonparenthetical_term("2 x y^2 3 2           3^2")
# parse_nonparenthetical_term("2 x -2") # -> warn
# parse_nonparenthetical_term("x")
# parse_nonparenthetical_term("-x") # error
# parse_nonparenthetical_term("+x") # correctly wrong
# parse_nonparenthetical_term("-5x")
# parse_nonparenthetical_term("-0x")
# parse_nonparenthetical_term("1.5x")
# parse_nonparenthetical_term("1.5^2x")
# parse_nonparenthetical_term("1e-2 x") # correctly error
parse_nonparenthetical_term <- function(string){
  
  # fix spaces around exponents "x ^ 2" -> "x^2"
  string <- str_replace_all(string, " *\\^ *", "^")
  
  # fix spaces around minuses "x  -  2" -> "x-2"
  string <- str_replace_all(string, " *- *", "-")
  
  # split based on spaces
  parts <- str_split(string, " ")[[1]]
  parts <- parts[nchar(parts) > 0] # for "2        -2"
  
  # if more than one negative provided error
  if(str_detect(str_sub(string, 2), fixed("-")))
    stop("Negative signs are only allowed at the beginning of terms.", call. = FALSE)
  
  # fix, e.g. "2x"
  smashed_var_bool <- str_detect(parts, "^[-. ^0-9]+[a-zA-Z]")
  if(any(smashed_var_bool)){
    places_to_break <- str_locate(parts[smashed_var_bool], "[a-zA-Z]")[,1]
    for(k in seq_along(places_to_break)){
      parts[smashed_var_bool][k] <- str_c(
        str_sub(parts[smashed_var_bool][k], 1, places_to_break[k]-1),
        "|",
        str_sub(parts[smashed_var_bool][k], places_to_break[k])
      )
    }
    parts <- unlist(str_split(parts, fixed("|")))
  }
  
  # fix, e.g. "-y"
  minus_var_bool <- str_detect(parts, "\\-[a-zA-Z]")
  if(any(minus_var_bool)){
    parts[minus_var_bool] <- str_c("-1 ", str_sub(parts[minus_var_bool], 2))
    parts <- unlist(str_split(parts, " "))
  }
  
  # collect numeric elements
  parts_with_vars <- str_detect(parts, "[a-zA-Z]")
  if(all(parts_with_vars)){
    coef <- 1L
  } else {
    coef <- prod(
      vapply(
        as.list(parts[which(!parts_with_vars)]), 
        function(.) eval(parse(text = .)),
        double(1)
      )  
    ) # this multiplies even, e.g., 5^2
  }
  
  # if only coefs are given, return
  if(all(parts_with_vars == FALSE)) return(mpoly(list(c(coef = coef))))
  
  # parse variable exponents
  var_parts <- parts[parts_with_vars]
  var_parts_with_exps_bool <- str_detect(var_parts, fixed("^"))
  var_parts[!var_parts_with_exps_bool] <- str_c(var_parts[!var_parts_with_exps_bool], "^1")
  var_parts <- str_split(var_parts, fixed("^"))
  vars <- vapply(var_parts, `[`, character(1), 1L)
  exps <- as.integer(vapply(var_parts, `[`, character(1), 2L))
  names(exps) <- vars
  
  # mpoly and return
  mpoly(list(c(coef = coef, exps)))  
}













# fix_term_joins("-2 - -2x + y - -3 y - 2")
# fix_term_joins("1e2x - 1e-2x + 1e+2x")
# fix_term_joins("1-1")
# fix_term_joins("x[1]")
# fix_term_joins("x[1,1]")
# fix_term_joins("1--1")
# fix_term_joins("1 - - 1")
# fix_term_joins("5 - 2     x")
# fix_term_joins("5^2x - 1")
# fix_term_joins("1+-xx-x")
# fix_term_joins("-1-1")
# fix_term_joins("1e-2 x")
# fix_term_joins("1e+2 x")
# fix_term_joins("1e2 x")
# fix_term_joins("-1-1-") # error
# fix_term_joins("-1-1+") # error
fix_term_joins <- function(string){
  
  # make sure last char is not a sign
  if(str_detect(string, "[+-]$")) stop(sprintf("Term %s does not terminate.", string), call. = FALSE)
  
  # zero trick for leading symbol, e.g. "-1 + x" -> "0 + -1 + x"
  if (str_detect(string, "^[+-]")) {
    if (str_detect(string, "^[+-]{2,}")) stop(
      sprintf("%s cannot start an expression.", str_extract(string, "^[+-]+")), 
      call. = FALSE
    )
    string <- str_c("0 + ", string)
  }
  
  # fix scientific notation
  sciRegex <- "[0-9.]+e[+-]?[0-9]+"
  while(str_detect(string, sciRegex)){
    stringToReplace <- str_extract(string, sciRegex)
    replacement <- format(as.numeric(stringToReplace))
    string <- str_replace(string, sciRegex, replacement)
  }
  
  # break string into pieces of terms and joins
  terms <- str_extract_all(string, "[\\w^.,|\\[\\]]+")[[1]]
  joins <- str_split(string, "[\\w^.,|\\[\\]]+")[[1]]
  if(joins[1] == "") joins <- joins[-1]
  if(joins[length(joins)] == "") joins <- joins[-length(joins)]
  if(length(joins) == 0L) return(string)
  
  # fix joins
  pureJoins <- str_replace_all(joins, "\\s", "")
  pureJoins[pureJoins == ""] <- "|"
  if(any(nchar(pureJoins) > 3)) stop("Arithmetic sign sequence of more than two detected.", call. = FALSE)
  cleanJoinMap <- c(
    "-" = " + -1 ",  "+" = " + ", "--" = " + ", 
    "++" = " + ", "+-" = " + -1 ", "-+" = " + -1 ", "|" = " "
  )
  cleanedJoins <- unname(cleanJoinMap[pureJoins]) # cbind(joins, cleanedJoins)
  
  # reconstruct
  n <- length(terms) + length(joins) # n always odd, first term always a \\w
  temp <- character(n)
  temp[seq.int(1L, n, 2L)] <- terms
  temp[seq.int(2L, n-1L, 2L)] <- cleanedJoins    
  string <- str_c(temp, collapse = "")
  
  # strip leading "0 + " if needed
  if(str_sub(string, 1L, 4L) == "0 + ") string <- str_sub(string, 5L)
  
  # return
  string
}
















# string <- "-1 (x + y)+ 2 x (x + y) + 3 y"
# string <- "2 (1 + x + (x - y))+ 2 x (x + y) + 3 y"
# extract_polynomial_terms(string)
extract_polynomial_terms <- function(string){
  
  # str_split(string, " *(?<!\\([\\w ]+)[+-] *")
  
  # run fix_term_joins on blanked strings to get protect parentheticals
  blanked_string <- blank_parentheticals(string, "|")
  piped_string   <- fix_term_joins(blanked_string)
  
  # change +"s to *"s for breaking later
  # they distinguish polynomial terms
  piped_string <- str_replace_all(piped_string, fixed("+"), "*")
  
  # unprotect
  string_ndcs <- str_locate_all(blanked_string, "[|]+")[[1]]
  piped_ndcs  <- str_locate_all(piped_string,   "[|]+")[[1]]  
  if(nrow(string_ndcs) > 0){
    for(k in 1:nrow(string_ndcs)){
      str_sub(piped_string, piped_ndcs[k,1], piped_ndcs[k,2]) <- 
        str_sub(string, string_ndcs[k,1], string_ndcs[k,2])  
    }
  }
  
  # split
  str_split(piped_string, fixed("*"))[[1]]
}






















# an inner parenthetical is one that does not contain parentheticals
# extract_leftmost_inner_parenthetical("(x + 5)")
# extract_leftmost_inner_parenthetical("(x + 5)", contents_only = TRUE)
#
# extract_leftmost_inner_parenthetical("(x + 5)^10")
# extract_leftmost_inner_parenthetical("(x + 5)^10", contents_only = TRUE)
# extract_leftmost_inner_parenthetical("((x + 5)^10+2)^2")
# extract_leftmost_inner_parenthetical("((x + 5)^10+2)", contents_only = TRUE)
# extract_leftmost_inner_parenthetical("(1 + (x + 5)^10+2)^2")
extract_leftmost_inner_parenthetical <- function(string, contents_only = FALSE){
  string <- str_extract(string, "\\([^()]*\\)(?:\\^[0-9]+)?")
  if(!contents_only) return(string)
  str_extract(string, "\\(.*\\)") ->.; str_sub(., 2L, -2L) 
}





# blank_parentheticals(" -1 1 x (3 x + -1 (7 + -1 2 x))^2 7 (x + 1) -3 ")
# blank_parentheticals(" -1 1 x (3 x + -1 (7 + -1 2 x))^2 7 (x + 1) -3 ", "*")
# blank_parentheticals(" -1 1 x (3 x + -1 (7 + -1 2 x))^2 7 (x + 1) -3 ", "_")
blank_parentheticals <- function(string, char = "-"){
  # " -1 1 x (3 x + -1 (7 + -1 2 x))^2 7 (x + 1) -3 " ->
  # " -1 1 x ------------------------- 7 ------- -3 "
  # this blanks parentheticals from the inside out
  # inside parentheticals are done first
  
  while(contains_parenthetical_expression(string)){
    bad <- extract_leftmost_inner_parenthetical(string)
    string <- str_replace(
      string, 
      "\\([^()]*\\)(?:\\^[0-9]+)?", 
      str_dup(char, nchar(bad))
    )
  }
  string
}







# string <- " -3 -1 (x + y)^2 4 (x - y)x 4 "
# extract_nonparenthetical_elements(string)
# string <- " (x + y)^2  (x - y)(x) "
# extract_nonparenthetical_elements(string)
extract_nonparenthetical_elements <- function(string){
  
  # remove parenthetical stuff
  parenthetical_regex <- "\\([-+*a-zA-Z0-9.^ ()]+\\)(\\^\\d+)?"
  nonparem_elts <- str_remove_all(string, parenthetical_regex)
  nonparem_elts <- str_replace_all(nonparem_elts, "\\s+", " ")
  nonparem_elts <- str_trim(nonparem_elts)
  
  # parenthesize and return
  if (nonparem_elts == "") {
    ""
  } else {
    str_c("(", str_trim(nonparem_elts), ")")
  }  
}

# string <- " -3 (x + y)^2 4 (x - y)x 4 "
# delete_nonparenthetical_elements(string)
# string <- " (x + y)^2 (x - y) (x) (y)  "
# delete_nonparenthetical_elements(string)
# string <- ".2 (x)"
# delete_nonparenthetical_elements(string)
extract_parenthetical_elements <- function(string){
  
  parenthetical_regex <- "\\([-+*a-zA-Z0-9.^ ()]+\\)(\\^\\d+)?"
  str_extract_all(string, parenthetical_regex)[[1]]
  
}



# string <- " -3 (x + y)^2 4 (x - y)(x)x 4 "
# term_parentheticals(string)
# string <- " -(x + y)^2   3(x - y)(x)  "
# term_parentheticals(string)
# string <- ".2 (x)"
# term_parentheticals(string) 
term_parentheticals <- function(string){
  
  nonparens <- extract_nonparenthetical_elements(string)
  parens    <-    extract_parenthetical_elements(string)
  c(nonparens, parens)
  
}










contains_parenthetical_expression <- function(string){ 
  any(str_detect(string, fixed("(")))
}




# contains_nested_parenthetical_expression("5+5")
# contains_nested_parenthetical_expression("(5+5)")
# contains_nested_parenthetical_expression("((5+5))")
# contains_nested_parenthetical_expression("x + (5 y) + 2")
# contains_nested_parenthetical_expression("x + ((5 y) + 2)")
contains_nested_parenthetical_expression <- function(string){
  only_parentheses <- str_replace_all(string, "[^()]", "")
  str_detect(only_parentheses, fixed("(("))
}



unmatched_parentheses_stop <- function(string){
  if(contains_parenthetical_expression(string)){
    open_paren_count <- str_count(string, fixed("("))
    closed_paren_count <- str_count(string, fixed(")"))
    if (open_paren_count > closed_paren_count){
      stop("Parenthetical error: excess ('s detected.", call. = FALSE)
    } else if(open_paren_count < closed_paren_count) {
      stop("Parenthetical error: excess )'s detected.", call. = FALSE)
    }
  }
  invisible()
}


empty_parenthetical_stop <- function(string) {
  if (str_detect(string, "\\( *\\)")) {
    stop("Expression contains empty parenthetical.", call. = FALSE)
  }
}



str_rev <- function(string) str_c(rev.default(str_split(string, "")[[1]]), collapse = "")






