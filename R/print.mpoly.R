#' Pretty printing of multivariate polynomials.
#' 
#' This is the major function used to view multivariate polynomials.
#' 
#' @param x an object of class mpoly
#' @param varorder the order of the variables
#' @param order a total order used to order the monomials in the 
#'   printing
#' @param stars print the multivariate polynomial in the more 
#'   computer-friendly asterisk notation (default FALSE)
#' @param silent logical; if TRUE, suppresses output
#' @param ... additional parameters to go to \code{\link{cat}}
#' @usage \method{print}{mpoly}(x, varorder, order, stars = FALSE,
#'   silent = FALSE, ...)
#' @return Invisible string of the printed object.
#' @export
#' @examples
#' p <- mp("2 x^5  -  3 y^2  +  x y^3")
#' p
#' print(p) # same
#' print(p, silent = TRUE)
#' s <- print(p, silent = TRUE)
#' s
#' 
#' print(p, order = "lex") # -> 2 x^5  +  x y^3  -  3 y^2
#' print(p, order = "lex", varorder = c("y","x")) # -> y^3 x  -  3 y^2  +  2 x^5
#' print(p, varorder = c("y","x")) # -> 2 x^5  -  3 y^2  +  y^3 x
#' 
#' print(p, stars = TRUE)
#' 
print.mpoly <- function(x, varorder, order, stars = FALSE, silent = FALSE, ...){
	
  ## argument checking and basic variable setting
  stopifnot(is.mpoly(x))  
  stopifnot(is.logical(stars)) 
  vars <- vars(x)
  
  
  
  ## deal with constant mpolys
  if(length(vars) == 0){
    if(!silent) cat(x[[1]], ...)
  	return(invisible(as.character(x[[1]])))
  }
  
    
  
  
  ## check variable order
  if(!missing(varorder)){
    stopifnot(is.character(varorder))
    if(!all(vars %in% varorder)) {
      stop(
        "if specified, varorder must contain all computed vars - ", 
        paste(vars, collapse = ", "),
        call. = FALSE
      )
    }
  } 
  
  if(!missing(order)) match.arg(order, c("lex", "glex", "grlex"))  
  
  
  
  
  ## reorder
  if(!missing(order) && !missing(varorder)){
    x <- reorder(x, varorder = varorder, order = order)
  } else if(!missing(varorder)){
    x <- reorder(x, varorder = varorder)
  } else if(!missing(order)){
    x <- reorder(x, order = order)	
  }
  
  
 
  
  ## print with stars or not, ^ coded first then *
  
  if(!stars){    # regular printing ^
  	
    ## make printed terms
    terms <- sapply(x, function(v){
      if(length(v) == 1) return(format(v[["coef"]], scientific=FALSE)) # if a constant term
      v <- v[v != 0] # eliminate lingering x^0"s
      p <- length(v) - 1
      s <- paste(names(v[1:p]), v[1:p], sep = "^", collapse = " ")
      s <- paste(format(v[["coef"]], scientific=FALSE), s)
      if(substr(s, 1, 2) == "1 ") s <- substr(s, 3, nchar(s))
      if(substr(s, 1, 3) == "-1 ") s <- paste("-", substr(s, 4, nchar(s)), sep = "")
      s
    })
  
    ## merge and pretty
    s <- paste(terms, collapse = "  +  ")
    s <- paste(" ", s, " ", sep = "") # insert " " pads
    s <- gsub("\\^1 ", " ", s)        # remove ^1"s  
    s <- gsub("\\+  -", "-  ", s)     # fix subtractions                       
    s <- substr(s, 2, nchar(s) - 1)   # remove " " pads
    if( substr(s, 1, 1) == "-" && x[[1]]["coef"] == -1 && length(x[[1]]) > 1 ){
	    s <- paste("-1", substr(s, 2, nchar(s)))
	  }
    if(!silent) cat(s, ...)
    return(invisible(s))
  
  } else {    # stars printing
  	
    ## make printed terms
    terms <- sapply(x, function(v){
      if(length(v) == 1) return(format(v[["coef"]], scientific=FALSE)) # if a constant term
      p <- length(v) - 1
      s <- paste(names(v[1:p]), v[1:p], sep = "**", collapse = " * ")
      s <- paste(format(v[["coef"]], scientific=FALSE), s, sep = " * ")
      if(substr(s, 1, 4) == "1 * ") s <- substr(s, 5, nchar(s))
      if(substr(s, 1, 5) == "-1 * ") s <- paste("-", substr(s, 6, nchar(s)), sep = "")
      s      
    })
  
    ## merge and pretty
    s <- paste(terms, collapse = "  +  ")
    s <- paste(" ", s, " ", sep = "") # insert " " pads
    s <- gsub("\\*\\*1 ", " ", s)     # remove ^1"s  
    s <- gsub("\\+  -", "-  ", s)     # fix subtractions      
    s <- substr(s, 2, nchar(s) - 1)   # remove " " pads
    if( substr(s, 1, 1) == "-" && x[[1]]["coef"] == -1 ){
	    s <- paste("-1 *", substr(s, 2, nchar(s)))
	  }    
    if(!silent) cat(s, ...)
    return(invisible(s))  	
      	
  }
}



