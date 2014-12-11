#' Define a multivariate polynomial.
#'
#' mp is a smart function which attempts to create a formal mpoly object from a character string containing the usual representation  of a multivariate polynomial.
#' 
#' @param string a character string containing a polynomial, see examples
#' @param varorder (optional) order of variables in string
#' @return An object of class mpoly.
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @seealso \code{\link{mpoly}}
#' @export mp
#' @examples
#' ( m <- mp('x + y + x y') )
#' is.mpoly( m )
#' unclass(m)
#'
#' mp('x - y')
#' mp('x - 1')
#' mp('x +      y')
#' mp('x -      5')
#' mp('x - -5')
#' mp('10 x 6 x') # -> 60 x^2
#' mp('10 x 6 x + 10 x 6 x y y 2') # -> 60 x^2  +  120 x^2 y^2
#'
#' mp('x^2 + x^2 y') # -> x^2  +  x^2 y
#'
#' mp('x - x') # -> 0
#' mp('x - 4 x') # -> -3 x
#' mp('x y^2 - y^2 x') # -> 0
#' 
#' mp('5^2') # -> 25
#' mp('2^2 x + 5^2 + 3^2') # -> 4 x  +  34
#' mp('1 1') # -> 1
#' mp('-1 -1 -1') # -> 1
#' mp('1  3 5^2 + 2 3^4 x') # -> 75  + 162 x
#' mp("x - 2 x -3") # 7 x
#'
#' ( ms <- mp(c('x + y', '2 x')) )
#' is.mpolyList(ms)
#'
#' mp('10 x + 2 y 3 + x^2 5 y') # -> 10 x  +  6 y  +  5 x^2 y
#' mp('x + 2 y + x^2 y + x y z') # -> x  +  2 y  +  x^2 y  +  x y z
#' mp('x + 2 y + x^2 y + x y z', varorder = c('y', 'z', 'x')) # -> x  +  2 y  +  y  +  y z x
#' #mp('x + 2 y + x^2 y', varorder = c('q', 'p')) # -> error
#'
#' mp('p111 + p121 2 p112^2')
#' unclass(mp('p111 + p121 2 p112^2'))
#'
#' mp('0')
#' mp('2')
#' mp('-5')
#' mp('-4 x')
#' mp('y + -1 x')
#' mp('-1 + x')
#' mp('-1 x')
#' mp('-1x')
#' mp('-x')
#' 
#' mp("(x)")
#' mp("((((x))))")
#' mp("(x + 0)")
#' mp("(x + 1)")
#' mp("(x - 1)")
#' mp("(-1 x - 1)")
#' mp("2 (x + 1)")
#' mp("-1 (x + 1)")
#' 
#' 
#' string <- "-2 x + -1 x (3 x - (7 - 2 x)) 7 (x + 1) -3"
#' mp(string)
#' # note that in the above problem, the -3 on the end is 
#' # times negative 3, not minus 3; that would be "... - 3"
#' 
#' mp("(x + 1) (x - 1)")
#' mp("(x + 1) (x + 2)")
#' mp("(x + 1)^5")
#' mp("x + 1")^5
#' mp("3 (x + 1) (x + 2)^2")
#'
#' mp("(x + y) (x - y)")
#' mp("((x + y) (x - y))^2")
#' mp("((x + y) (x - y)^2)")
#' mp("((x + y) (x - y)^2)^2")
#'
#' mp(c("x","x + y"))
#'
#'
#'
#'
#' gradient( mp("x + 2 y + x^2 y + x y z") ) 
#' gradient( mp("(x + y)^10") ) 
#'
#' # mp and the print methods are kinds of inverses of each other
#' ( polys <- mp(c('x + y', 'x - y')) )
#' strings <- print(polys)
#' strings
#' mp(strings)
#' 
#' 
#' 
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' # working new in version 0.6
#' mp('x + 4x') # note the 4x as opposed to 4 x
#' mp('x - 4x') # same
#' mp('x -1') # -> -1 x
#' mp("x(1+x)")
#'
#' mp("x ((x + y) + 2)")
#' mp("1+x")
#' 
#' 
#' 
#' 
mp <- function(string, varorder){
  
  stopifnot(is.character(string))
  
  # if string is a vector of polys, return mpolyList
  if(length(string) > 1){
    if(missing(varorder)){
      mpolyList <- lapply(
        as.list(string),
        mp
      )
    } else {
      mpolyList <- lapply(
        as.list(string),
        mp,
        varorder = varorder
      )    	
    }
    class(mpolyList) <- 'mpolyList'
    return(mpolyList)
  }
  
  # check for unmatched parentheses
  if(any(str_detect(string, fixed(c("(",")"))))){
    if(str_count(string, fixed("(")) != str_count(string, fixed(")"))){
      stop("not all parenthetical expressions closed.", call. = FALSE)
    }
    # check for exponents of parentheticals
    if(str_detect(string, fixed(")^"))) string <- parenExp(string)
  }
  
  # compute
  out <- parse_parenthetical_polynomial(string)
  
  # check varorder argument
  if(!missing(varorder)){
    
    if(!all(vars %in% varorder)){
      error <- paste(
        'if specified, varorder must contain all computed vars - ',
        paste(vars, collapse = ', '),
        sep = ''
      )
      stop(error, call. = FALSE)
    }
    
    # order vars appropriately
    vars <- intersect(varorder, vars)
    out <- reorder.mpoly(out, varorder = vars)
  } 
  
  # return
  out
}









# parse_parenthetical_polynomial("x ((x+y)+2)")
# parse_parenthetical_polynomial("x ((x+y) + 2)")
# parse_parenthetical_polynomial("(x + y) + 2 x (x + y) + 3 y")
parse_parenthetical_polynomial <- function(string){
  
  # convert minuses to pluses
  # is a minus is followed by a space, it's subtraction
  # otherwise, it's times -1
  string <- str_replace_all(string, fixed("- "), "+ -1 ")

  # locate parenthetical-containing terms
  plusNdcs <- str_locate_all(blank_parentheticals(string), fixed("+"))[[1]][,1]
  
  # locate true term +s and split on them
  for(k in plusNdcs) str_sub(string, k, k) <- "|"
  parenthetical_terms <- str_trim(str_split(string, fixed("|"))[[1]])
  
  # parse into mpolys
  mpolys <- lapply(as.list(parenthetical_terms), parse_parenthetical_term)
  
  # add and return
  Reduce("+", mpolys)
}

















#' parse_parenthetical_term(" 3 (x + y) 4 (x - y) ")
#' parse_parenthetical_term("(x + y) (x - y)")
#' parse_parenthetical_term("(x + y)")
#' parse_parenthetical_term("((5^2))")
parse_parenthetical_term <- function(string){
  
  # " 3 (x + y) 4 (x - y) " -> " (3 4) (x + y) (x - y) "
  string <- str_pad(string, nchar(string)+2, "both")
  fLP <- blank_parentheticals(string)   
  
  if(str_detect(fLP, "[:alnum:]")){
  
    paren <- str_replace_all(fLP, " [-]{2,}", "")
    paren <- paste0("(", str_trim(paren), ")")
      
    # grab parenthetical expressions from left to right
    while(str_detect(fLP, fixed("--"))){
    
      # detect first paren start and stop
      startParens <- unname(str_locate(fLP, fixed(" --"))[1,1] + 1)
      endParens <- unname(str_locate(fLP, fixed("-- "))[1,1] + 1)
    
      # append to paren vector
      paren <- c(paren, str_sub(string, startParens, endParens))
    
      # remove from the string
      str_sub(string, startParens, endParens) <- ""        
      str_sub(fLP, startParens, endParens) <- ""    
    
    }
    
  } else {
    
    string <- str_replace_all(string, "\\)[ ]*\\(", ")|(")
    paren <- str_split(string, fixed("|"))[[1]]
    
  }
  
  paren <- str_trim(paren)
  
  mpolys <- lapply(as.list(paren), function(t){
    piece <- str_sub(t, 2, -2)
    if(str_detect(piece, fixed("("))){
      parse_parenthetical_polynomial(piece)
    } else {
      parse_nonparenthetical_polynomial(piece)
    }
  })
  
  Reduce("*", mpolys)
}





















#' string <- "-4 + 2+2 x +   1 x y^4 -3 prq^3 -y - 3 x 2 - 3 y -2"
#' parse_nonparenthetical_polynomial(string)
#' parse_nonparenthetical_polynomial("x    +       y")
parse_nonparenthetical_polynomial <- function(string){
  
  # convert minuses to pluses
  # is a minus is followed by a space, it's subtraction
  # otherwise, it's times -1
  string <- str_replace_all(string, fixed("- "), "+ -1 ")
  
  # trim white space
  string <- str_trim(string)
  
  # split polynomial
  terms <- str_trim(str_split(string, fixed("+"))[[1]])
  
  # parse terms
  mpolyTerms <- lapply(as.list(terms), parse_nonparenthetical_term)
  
  # combine and return
  Reduce("+", mpolyTerms)
}



#' parse_nonparenthetical_term("12var 2 y 2x")
#' parse_nonparenthetical_term("2      -2")
#' parse_nonparenthetical_term("2 x y^2 3 -1           3^2")
#' parse_nonparenthetical_term("x")
parse_nonparenthetical_term <- function(string){
  
  string <- str_trim(string)
  parts <- str_split(string, " ")[[1]]
  parts <- parts[nchar(parts) > 0] # for "2        -2"
  
  # fix, e.g. "2x"
  smashed_var_bool <- str_detect(parts, "[0-9]+[:alpha:]")
  if(any(smashed_var_bool)){
    places_to_break <- str_locate(parts[smashed_var_bool], "[:alpha:]")[,1]
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
  minus_var_bool <- str_detect(parts, "\\-[:alpha:]")
  if(any(minus_var_bool)){
    parts[minus_var_bool] <- str_c("-1 ", str_sub(parts[minus_var_bool], 2))
    parts <- unlist(str_split(parts, " "))
  }
  
  # collect numeric elements
  parts_with_vars <- str_detect(parts, "[:alpha:]")
  if(all(parts_with_vars)){
    coef <- 1L
  } else {
    coef <- prod(
      sapply(
        as.list(parts[which(!parts_with_vars)]), 
        function(.) eval(parse(text = .))
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
  vars <- sapply(var_parts, function(x) x[1])
  exps <- sapply(var_parts, function(x) as.integer(x[2]))
  names(exps) <- vars
  
  # mpoly and return
  mpoly(list(c(coef = coef, exps)))  
}














blank_parentheticals <- function(s){
  # " -1 1 x (3 x + -1 (7 + -1 2 x)) 7 (x + 1) -3 " ->
  # " -1 1 x ----------------------- 7 ------- -3 "
  while(str_detect(s, fixed("("))){
    bad <- str_extract(s, "\\([^()]*\\)") 
    s <- str_replace(s, "\\([^()]*\\)", str_dup("-", nchar(bad)))
  }
  s  
}