library("stringr")



## mpoly_terms
############################################################

print.mpoly_term <- function (term, silent = FALSE, stars = FALSE) {

  # treat constant terms (no indeterminates) and those with vars differently
  if (length(term$core) == 0L) { 
    
    printed_string <- str_sub(capture.output(print(term$coef)), 5)
    
  } else {

    # set times sign
    times <- if(stars) "*" else " "
    
    # format coefficient
    coef_to_disp <- if (class(term$coef) == "complex") {
      if (Im(term$coef) == 0) { # if purely real
        str_sub(capture.output(print(Re(term$coef))), 5)
      } else if (Re(term$coef) == 0) { # if purely imaginary
        str_c(str_sub(capture.output(print(Im(term$coef))), 5), "i")
      } else {
        str_c("(", str_sub(capture.output(print(term$coef)), 5), ")")
      }
    } else {
      str_sub(capture.output(print(term$coef)), 5)
    }  
    
    # format indeterminates, e.g. "x^1 y^2" or "x^1*y^2"
    core_to_disp <- str_c(
      names(term$core), term$core, 
      sep = "^", collapse = times
    )
    
    # remove ^1's
    if (any(term$core == 1L)) {
      core_to_disp <- str_remove_all(core_to_disp, "\\^1(?![0-9])")
    }
    
    # put coef and core together, unless coef = 1
    if ( term$coef == 1 && length(term$core) != 0 ) {
      printed_string <- core_to_disp
    } else {
      printed_string <- str_c(coef_to_disp, core_to_disp, sep = times)
    }  
    
  }  
  
  # print/return
  if(!silent) cat(printed_string)
  invisible(printed_string)
}


## examples
########################################


term <- structure(
  list(
    coef = 2L,
    core = c("x" = 1L, "y" = 2L, "z" = 1L)
  ),
  class = "mpoly_term"
)

# term
# 2 x y^2 z
# print(term, stars = TRUE)
# 2*x*y^2*z


structure(
  list(
    coef = complex(real = 1, imaginary = 1),
    core = c("x" = 1L, "y" = 2L)
  ),
  class = "mpoly_term"
)
# (1+1i) x y^2




structure(
  list(
    coef = complex(real = -1, imaginary = 0),
    core = c("x" = 1L, "y" = 2L)
  ),
  class = "mpoly_term"
)
# -1 x y^2



structure(
  list(
    coef = complex(real = 0, imaginary = -2),
    core = c("x" = 1L, "y" = 2L)
  ),
  class = "mpoly_term"
)
# -2i x y^2


structure(
  list(
    coef = -5,
    core = integer(0)
  ),
  class = "mpoly_term"
)
# -5













## bare_mpoly's
############################################################

print.bare_mpoly <- function (x, silent = FALSE, ...) {
  
  # print each term
  terms <- vapply(x, print.mpoly_term, character(1), silent = TRUE)
  
  # merge printed terms
  printed_string <- str_c(terms, collapse = " + ")
  
  # change " + -1 "'s to " - "'s
  printed_string <- str_replace_all(printed_string, " \\+ -1[ *]", " - ")
  
  # print/return
  if(!silent) cat(printed_string)
  invisible(printed_string)
  
}



## examples
########################################

structure(
  list(
    structure(
      list(
        coef = complex(real = -5, imaginary = 1),
        core = integer(0)
      ),
      class = "mpoly_term"
    ),
    structure(
      list(
        coef = complex(real = -1),
        core = c("x" = 2L, "y" = 1L)
      ),
      class = "mpoly_term"
    ),
    structure(
      list(
        coef = complex(real = -1),
        core = c("x" = 2L, "y" = 1L)
      ),
      class = "mpoly_term"
    )
  ),
  class = "bare_mpoly"
)
# -5+1i - x^2 y - x^2 y



structure(
  list(
    structure(
      list(
        coef = -5,
        core = integer(0)
      ),
      class = "mpoly_term"
    ),
    structure(
      list(
        coef = -1,
        core = c("x" = 2L, "y" = 1L)
      ),
      class = "mpoly_term"
    )
  ),
  class = "bare_mpoly"
)
# -5 - x^2 y



## mpoly
############################################################


print.mpoly <- function (x, silent = FALSE, ...) {
  
  # make printouts of bare_mpolys
  printed_strings <- vapply(x, print.bare_mpoly, character(1), silent = TRUE)
  
  # add back dimensions (if dimensionless, assumed to be a column/row vector)
  if (!is.null(dim(x))) dim(printed_strings) <- dim(x)
  
  # replace "'s with spaces
  printed_strings <- capture.output(print(printed_strings))
  printed_strings <- str_replace_all(printed_strings, "(?<= )\"", "")
  printed_strings <- str_replace_all(printed_strings, "\"(?= |$)", "  ")
  
  # print/return
  if(!silent) cat(printed_strings, sep = "\n")
  invisible(printed_strings)
  
}



structure(
  list(
    structure(
      list(
        coef = -5,
        core = integer(0)
      ),
      class = "mpoly_term"
    ),
    structure(
      list(
        coef = -1,
        core = c("x" = 2L, "y" = 1L)
      ),
      class = "mpoly_term"
    )
  ),
  class = "bare_mpoly"
) -> p


structure(
  list(p, p, p, p),
  class = "mpoly", 
  coefring = "numeric", 
  vars = c("x", "y"),
  .Dim = c(2L,2L)
)
#      [,1]         [,2]        
# [1,] -5 - x^2 y   -5 - x^2 y  
# [2,] -5 - x^2 y   -5 - x^2 y


structure(
  list(p, p, p, p, p, p, p, p),
  class = "mpoly", 
  coefring = "numeric", 
  vars = c("x", "y"),
  .Dim = c(2L,2L,2L)
)
# , , 1
# 
#      [,1]         [,2]        
# [1,] -5 - x^2 y   -5 - x^2 y  
# [2,] -5 - x^2 y   -5 - x^2 y  
# 
# , , 2
# 
#      [,1]         [,2]        
# [1,] -5 - x^2 y   -5 - x^2 y  
# [2,] -5 - x^2 y   -5 - x^2 y




