#' REMOVED -- Compute a grobner basis of a list of multivariate 
#' polynomials.
#' 
#' This function has been removed to eliminate mpoly's dependence on
#' packages that only it uses.  To compute a Grobner basis of a 
#' collection of multivariate polynomials, checkout the new m2r
#' package, which you can download with the code in the first example.
#' 
#' grobner computes a Grobner basis for a collection of multivarite 
#' polynomials represented as an object of class mpolyList. Note 
#' that the polynomials printed after calculation are unlikely to be
#' properly ordered; this is because the order of the monomials 
#' displayed is governed by the print method, not the mpoly's 
#' themselves.
#' 
#' @param mpolyList an object of class mpolyList
#' @param varorder order of variables
#' @param order total order to be considered for monomials (e.g. 
#'   lexicographic)
#' @return An object of class mpolyList.
#' @export
#' @examples
#' \dontrun{
#' 
#' 
#' # code to download m2r:
#' # note that to do this you should have Macaulay2 installed,
#' # see https://github.com/musicman3320/m2r and 
#' # http://www.math.uiuc.edu/Macaulay2/Downloads/
#' if(!require(devtools)) install.packages("devtools")
#' devtools::install_github("musicman3320/m2r")
#' 
#' 
#' 
#' 
#' 
#' }
grobner <- function(mpolyList, varorder = vars(mpolyList), order = 'lex'){
  
  .Defunct("gb", "m2r",
    paste0(
      "grobner is no longer part of the mpoly package.\n",
      "  See ?grobner for other ways to compute a Grobner basis."
    )
  )
  
  stopifnot(is.mpolyList(mpolyList))

  # determine variables
  variables <- varorder
  varsTogether <- paste(variables, collapse = ', ')      
  if(missing(varorder)) {
    message <- paste("using variable ordering - ", varsTogether, sep = "")
    message(message)
  }  
  
  # initialize sympy variables
  # lapply(as.list(variables), Var) # Var uses sympy
  
  # error if varorder is not a permutation of detected variables
  if(!missing(varorder) && !all(vars(mpolyList) %in% varorder)){
  	error_message <- paste(
  	  'if specified, varorder must contain all computed vars -',
  	  paste(vars(mpolyList), collapse = ', ')
  	)
    stop(error_message, call. = FALSE)	
  }
  
  # create character versions of polynomials
  # proper syntax : (after Var('t'), ..., run)
  # sympy('groebner ([t ** 4 - x, t ** 3 - y, t ** 2 - z], t, x, y, z, order = \"lex\")')  
  poly_chars <- suppressMessages(sapply(mpolyList, print.mpoly, stars = TRUE))
  polysTogether <- paste(poly_chars, collapse = ', ')
  pre_grob <- paste(
    paste('groebner([', polysTogether, '], ', sep = ''),
    varsTogether,
    paste(', order = "', order, '")', sep = ''),
    sep = ''
  )
  # gb <- sympy(pre_grob) # sympy dependent
  gb <- substr(gb, 2, nchar(gb) - 1)
  gb <- gsub('\\*\\*', '\\^', gb) # issue : rational expressions allowed, e.g. "x - 2*y^3/3"
  gb <- gsub('\\*', ' ', gb)    
  
  # fixing rational expressions
  gb <- strsplit(gb, split = ', ')[[1]]  
  gb <- gsub(' \\- ', ' + -', gb)
  gb_split <- strsplit(gb, split = ' \\+ ')  
  gb_split <- lapply(gb_split, strsplit, split = '/')
  gb <- sapply(gb_split, function(l){  # e.g. l = list("x", c("-2*y^3", "3")) (the terms)
  	coefs <- NULL
  	monomials <- NULL
  	divisors_in_polynomial <- NULL
    for(k in 1:length(l)){
      s <- strsplit(l[[k]][1], ' ')[[1]]
      
      if( str_detect(s[1], '[a-zA-Z]+') ){ # no coefficient
      	if(str_detect(s[1], '-')){  # e.g. -x
      	  coefs <- c(coefs, -1)
      	  s[1] <- gsub('-', '', s[1])
          monomials <- c(monomials, paste(s, collapse = ' '))
      	} else {
      	  coefs <- c(coefs, 1)	
          monomials <- c(monomials, paste(s, collapse = ' '))
      	}
      } else {  # coefficient present
      	coefs <- c(coefs, as.numeric(s[1]))
      	if(length(s) > 1){
          monomials <- c(monomials, paste(s[2:length(s)], collapse = ' '))
        } else {
          monomials <- c(monomials, '')
        }
      }
    	
      # grab quotients
      if(length(l[[k]]) == 1){ 
        divisors_in_polynomial <- c(divisors_in_polynomial, 1)
      } else {
        divisors_in_polynomial <- c(divisors_in_polynomial, as.numeric(l[[k]][2]))
      }
    }
    lcmOfDivisors <- Reduce(LCM, divisors_in_polynomial)
    coefs <- lcmOfDivisors * coefs / divisors_in_polynomial
    out <- paste(paste(coefs, monomials, sep = ' '), collapse = ' + ')    
    gsub('\\+ \\-', '- ', out)
  })
  
  # format and return
  gb <- lapply(as.list(gb), mp)
  class(gb) <- 'mpolyList'

  # note that the order of the monomials in each resulting gb polynomial 
  # will likely appear incorrectly ordered.  this is because the order is 
  # provided by the print method, not the mpoly's themselves.
  
  
  gb
}

