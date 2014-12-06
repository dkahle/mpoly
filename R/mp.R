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
#'
#'
#' # possible specification syntax issues -
#' mp('x + 4x') # note the 4x as opposed to 4 x
#' mp('x - 4x') # same
#' mp('x -1') # -> -1 x
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
  
  
  # trim string
  string <- str_trim(string)
  
  
  # do parenthetical expressions
  if(any(str_detect(string, c("\\(","\\)")))){
    if(str_count(string, "\\(") != str_count(string, "\\)")){
      stop("not all parenthetical expressions closed.", call. = FALSE)
    }
    # check for exponents of parentheticals
    if(str_detect(string, "\\)\\^")) string <- parenExp(string)
  }


  # constants - no variables, allows mp('-5') mp('5') mp('3^2') mp('2 1')
  if(!str_detect(string, "[a-zA-Z]")){
  	if(str_detect(string, "\\+")){
  	  string <- str_trim(strsplit(string, "\\+")[[1]])
  	}
    string <- gsub('[ ]{2,}', ' ', string)
    string <- gsub('[ ]{1}', '*', string)
    string <- paste(string, collapse = "+")  	
    return(mpoly( list(c(coef = eval(parse(text = string)))) ))
  }

 
  # fix negatives
  if(substr(string, 1, 1) == '-'){ # starting with a negative coefficient
    string <- paste('0 + ', string, sep = '')
  }
  string <- gsub('\\+ \\-', '- ', string) # negative coefficients
  string <- gsub('\\- \\-', '+ ', string) # minus negative
  string <- gsub(' \\- ', ' + -1 ', string) # subtraction
  
  # fix white space (shrink x +   y to x + y)
  string <- gsub('[ ]{2,}', ' ', string)

  
  # division
  if(str_detect(string, '/')){
    stop('expressions with division are not currently handled (use decimals).')
  }  
  
  
  ## begin real work
   
  # prep for lapply to come
  string <- paste(' ', string, ' ', sep = '') 
  
  
  # do parenthetical expressions  
  if(str_detect(string, "\\(")){

  	stringTmp <- string
    pluses <- str_locate_all(flatLineParentheticals(stringTmp), " \\+ ")[[1]]
    if(nrow(pluses) > 0){
      for(k in 1:nrow(pluses)){
        str_sub(stringTmp, pluses[k,"start"], pluses[k,"end"]) <- " | "
      }
      l <- as.list(strsplit(stringTmp, " \\| ")[[1]])
      l <- lapply(l, function(x){
        x <- str_trim(x)
        str_pad(x, nchar(x) + 2, "both")
      })
    } else {
      l <- as.list(stringTmp)
    } 

    l <- lapply(l, function(s){
      if(!str_detect(s, "\\(")) return(mp(s))
      Reduce(
        "*",
        lapply(as.list(bubble(s)), mp)
      )
    })
    return(Reduce("+", l))
        
  } else {
    l <- as.list(strsplit(string, '\\+')[[1]]) 
  }

  terms <- lapply(l, function(s){
    n <- nchar(s)
    substr(s, 2, n - 1)	
  })
  
  # burst terms
  elements <- lapply(terms, function(string){
    strsplit(string, ' ')[[1]]
  })
  if(length(elements[[1]]) == 1 && elements[[1]] == '0'){ # fix - first coef
  	elements <- elements[2:length(elements)]
  }

  # fix exponents, division, multiplication of constants
  parseNumberElem <- function(x) as.character(eval(parse(text = x)))
  
  elements <- lapply(elements, function(x){

  	varElems <- str_detect(x, "[a-zA-Z]")
  	
  	# parse only number elements e.g. c("1", "3", "5^2")
  	if(!any(varElems)){
  	  x <- paste(x, collapse = "*")
  	  return(parseNumberElem(x))
  	}
  	
  	# combine coefficient elements
  	xCoefs <- x[!varElems]
  	xCoef <- parseNumberElem(paste(xCoefs, collapse = "*"))
  	
  	#
  	c(xCoef, x[varElems])
  })

  
  # determine variables
  pre_vars <- unlist(elements)
  var_ndxs <- substr(pre_vars, 1, 1) %in% c(letters, LETTERS)
  pre_vars <- unique(pre_vars[var_ndxs])
  pre_vars <- gsub('\\^[0-9]+', '', pre_vars) # remove exponents
  vars <- unique(pre_vars)

  
  # set number of variables in string
  p <- length(vars)
  
  
  # populate data frame for mpoly
  l <- lapply(elements, function(v){
  	
    # get coefficient (the code takes care of missing coefs using defaults)
    asNum <- suppressWarnings(as.numeric(v))
    coef <- prod(asNum, na.rm = TRUE)
    if(sum(is.na(asNum)) > 0){ 
      v <- v[is.na(asNum)]
    } else {
      return(c(coef = coef))	
    }

    # parse degrees
    v <- sapply(strsplit(v, '\\^'), function(z){
      if(length(z) == 1) z <- c(z, 1)
      out <- as.numeric( z[2] )
      names(out) <- z[1]
      out
    })

    c(v, coef = coef)
  })
  

  # mpoly 
  out <- mpoly(l)
  
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






 
flatLineParentheticals <- function(s){
  # " -1 1 x (3 x + -1 (7 + -1 2 x)) 7 (x + 1) -3 " ->
  # " -1 1 x ----------------------- 7 ------- -3 "
  while(str_detect(s, "\\(")){
    bad <- str_extract(s, "\\([^()]*\\)") 
    s <- str_replace(s, "\\([^()]*\\)", str_dup("-", nchar(bad)))
  }
  s  
}




bubble <- function(s){
  # bubble out non-parentheticals
  # " -1 1 x (3 x + -1 (7 + -1 2 x)) 7 (x + 1) -3 " ->
  # " (-1 1 x 7 -3) (3 x + -1 (7 + -1 2 x)) (x + 1) "
  fLP <- flatLineParentheticals(s)
  bubbled <- str_replace_all(fLP, " [-]{2,}", "")
  bubbled <- paste0("(", str_trim(bubbled), ")")
  if(!str_detect(bubbled, "[[:alnum:]]")) bubbled <- "()"

  paren <- bubbled
  while(str_detect(fLP, "--")){
    startParens <- unname(str_locate(fLP, " --")[1,1] + 1)
    endParens <- unname(str_locate(fLP, "-- ")[1,1] + 1)
    paren <- c(paren, str_sub(s, startParens, endParens))
    str_sub(s, startParens, endParens) <- ""        
    str_sub(fLP, startParens, endParens) <- ""    
  }
  if(length(which(paren == "()")) > 0)
    paren <- paren[-which(paren == "()")]
  sapply(as.list(paren), function(t) str_sub(t, 2, nchar(t)-1))
}





parenExp <- function(s){
  
  while(str_detect(s, "\\)\\^")){
  	
  	parenToFix <- str_extract(s, "\\([^()]*\\)\\^[0-9]+")   
  	# e.g. "(2 x - 7)^2"  	  	
  	if(!is.na(parenToFix)){
      split <- strsplit(parenToFix, "\\)\\^")[[1]]
      split[1] <- str_sub(split[1], 2)
      r <- paste(paste0("(", 
        rep(split[1], as.integer(split[2])),
        ")"
      ), collapse = " ")
      s <- str_replace(s, "\\([^()]*\\)\\^[0-9]+", r)
    } else {
  	  parenToFix <- str_extract(s, 
  	    "\\([[:alnum:][:space:][:punct:]]*\\)\\^[0-9]+")     		
      split <- strsplit(parenToFix, "\\)\\^")[[1]]
      split[1] <- str_sub(split[1], 2)
      r <- paste(paste0("(", 
        rep(split[1], as.integer(split[2])),
        ")"
  	  ), collapse = " ")  	    
      s <- str_replace(s, "\\([[:alnum:][:space:][:punct:]]*\\)\\^[0-9]+", r)  	  
    }
    
  }
  
  s
}