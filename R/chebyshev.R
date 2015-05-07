#' Chebyshev polynomials
#' 
#' Chebyshev polynomials
#' 
#' @param degree degree of polynomial
#' @param kind 1 or 2 (Chebyshev polynomials of the first and second kinds)
#' @param indeterminate indeterminate
#' @return a mpoly object
#' @author David Kahle modifying code from Adam Leerich modifying code from Hadley Wickham
#' @export
#' @examples
#' 
#' chebyshev(0)
#' chebyshev(1)
#' chebyshev(2)
#' chebyshev(3)
#' chebyshev(4)
#' chebyshev(5)
#' chebyshev(6)
#' chebyshev(10)
#' 
#' chebyshev(0:5)
#' chebyshev(0:5, 2)
#' 
#' chebyshev(0:5, indeterminate = "t")
#' 
#' 
#' \dontrun{  # visualize the chebyshev polynomials
#' 
#' library(ggplot2); theme_set(theme_bw())
#' library(reshape2)
#' 
#' s <- seq(-1, 1, length.out = 201)
#' N <- 5 # number of chebyshev polynomials to plot
#' (chebPolys <- chebyshev(0:N))
#' 
#' df <- t(sapply(s, as.function(chebPolys)) )
#' df <- as.data.frame(cbind(s, df))
#' names(df) <- c("x", paste0("T_", 0:N))
#' mdf <- melt(df, id = "x")
#' qplot(x, value, data = mdf, geom = "line", color = variable)
#' 
#' 
#' }
#'
chebyshev <- function(degree, kind = 1, indeterminate = "x"){
  
  stopifnot(all(is.wholenumber(degree)))
  stopifnot(all(degree >= 0))
  
  
  ## deal with kind
  if(kind == 1){
    multiplier <- 1
  } else if(kind == 2){
    multiplier <- 2
  } else{
    stop("only kinds 1 and 2 are allowed.", call. = FALSE)
  }
  

  ## set up memoise infrastructure
  cache <- new.env(TRUE, emptyenv())
  
  cache_set <- function(key, value) assign(key, value, envir = cache)
  
  cache_get <- function(key) get(key, envir = cache, inherits = FALSE)
  
  cache_has_key <- function(key) exists(key, envir = cache, inherits = FALSE)
  
  
  ## set base cases
  cache_set("0", mp("1"))
  cache_set("1", multiplier * mp(indeterminate))
    
  
  ## write function
  cheb <- function(n){    
    
    # deal with vectorized n
    if(length(n) > 1){ 
      listOPolys <- lapply(n, function(.) cheb(.))
      class(listOPolys) <- "mpolyList"
      return(listOPolys)    
    }
    
    # set n as character (convenience)
    nc <- as.character(n)
    
    # if result known, return
    if(cache_has_key(nc)) return(cache_get(nc))
    
    # compute result
    out <- 2*mp(indeterminate)*cheb(n-1) - cheb(n-2)
    
    # cache result
    cache_set(nc, out)
    
    # return
    out    
  } 
   
  ## make it and run it
  cheb(degree)
}



