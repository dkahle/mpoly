#' Convert an object to an mpoly
#'
#' mpoly is the most basic function used to create objects of class mpoly.
#'
#' @param x an object
#' @param ... additional arguments to pass to methods
#' @return the object formated as a mpoly object.
#' @author David Kahle \email{david@@kahle.io}
#' @seealso [mp()]
#' @export
#' @examples
#'
#' library(ggplot2); theme_set(theme_classic())
#' library(dplyr)
#'
#' n <- 101
#' s <- seq(-5, 5, length.out = n)
#'
#'
#'
#' # one dimensional case
#'
#' df <- data.frame(x = seq(-5, 5, length.out = n)) %>%
#'   mutate(y = -x^2 + 2*x - 3 + rnorm(n, 0, 2))
#'
#' (mod <- lm(y ~ x + I(x^2), data = df))
#' (p <- as.mpoly(mod))
#' qplot(x, y, data = df) +
#'   stat_function(fun = as.function(p), colour = "red", size = 1)
#'
#' (mod <- lm(y ~ poly(x, 2, raw = TRUE), data = df))
#' (p <- as.mpoly(mod))
#' qplot(x, y, data = df) +
#'   stat_function(fun = as.function(p), colour = "red", size = 1)
#'
#' (mod <- lm(y ~ poly(x, 1, raw = TRUE), data = df))
#' (p <- as.mpoly(mod))
#' qplot(x, y, data = df) +
#'   stat_function(fun = as.function(p), colour = "red", size = 1)
#'
#'
#'
#' # two dimensional case with ggplot2
#'
#' df <- expand.grid(x = s, y = s) %>%
#'   mutate(z = x^2 - y^2 + 3*x*y + rnorm(n^2, 0, 3))
#' qplot(x, y, data = df, geom = "raster", fill = z)
#'
#' (mod <- lm(z ~ x + y + I(x^2) + I(y^2) + I(x*y), data = df))
#' (mod <- lm(z ~ poly(x, y, degree = 2, raw = TRUE), data = df))
#' (p <- as.mpoly(mod))
#' df$fit <- apply(df[,c("x","y")], 1, as.function(p))
#'
#' qplot(x, y, data = df, geom = "raster", fill = fit)
#'
#' qplot(x, y, data = df, geom = "raster", fill = z - fit) # residuals
#'
#'
#'
#'
#'
#'
#' 
as.mpoly <- function(x, ...) UseMethod("as.mpoly")



#' @export  
as.mpoly.default <- function(x, ...) {
  stop(sprintf("objects of class %s not supported by as.mpoly().", class(x)), call. = FALSE)
}






#' @export  
as.mpoly.alm <- function(x, ...) {
  
  names <- names(coef(x))
  if ("(Intercept)" %in% names) {
    names[names == "(Intercept)"] <- "1"
  }
  
  string <- paste(
    unname(coef(x)),
    names,
    sep = "*",
    collapse = " + "
  )
  
  mp(string, stars_only = TRUE)
  
}





#' @export  
as.mpoly.lm <- function(x, ...){
  
  coefs <- coef(x)
  coef_names <- names(coefs)
  coef_names[coef_names == "(Intercept)"] <- 1
  
  # check for "poly(x, 2)1" syntax
  if(any(str_detect(coef_names, "poly"))) {
    
    poly_ndcs <- which(str_detect(coef_names, "poly"))  
    coef_names[poly_ndcs] <- vapply(coef_names[poly_ndcs], parse_model_poly, character(1))
    mp_str <- paste(coefs, coef_names, sep = " ", collapse = " + ")
    
  } else { # I() notation
    
    I_ndcs <- which(str_detect(coef_names, "I([0-9a-zA-Z]*)"))  
    if(length(I_ndcs) > 0){
      coef_names[I_ndcs] <- vapply(
        coef_names[I_ndcs], 
        function(s) str_sub(s, 3, -2),
        character(1)
      )
    }
    coef_names  <- str_replace_all(coef_names, " \\* ", " ")
    mp_str <- paste(coefs, coef_names, sep = " ", collapse = " + ")
    
  }
  

  mp(mp_str)
}



# s <- "poly(x, y, degree = 2, raw = TRUE)2.0"
# parse_model_poly(s)
parse_model_poly <- function(s) {

  # grab vars; check for raw
  inside <- str_extract_all(s, "poly\\(.+\\)")[[1]]
  inside <- str_sub(inside, 6, -2)
  inside <- str_split(inside, ", ")[[1]]
  if(!any(inside == "raw = TRUE")) {
    stop("poly() statements currently must contain raw = TRUE.", call. = FALSE)
  }
  vars <- inside[!(str_detect(inside, "=") | !str_detect(inside, "[:alpha:]"))]
  
  # parse exponents
  if(str_sub(s, -1) == ")") {
    exponents <- 1
  } else {
    exponents <- str_sub(str_extract_all(s, "\\).+")[[1]], 2)
    if(length(exponents) == 0) exponent <- "1"
    exponents <- str_split(exponents, "\\.")[[1]]
  }
  
  # put together
  paste(paste(vars, exponents, sep = "^"), collapse = " ")
}





#' @export  
as.mpoly.polynomial <- function(x, indeterminate = "x", ...){
  as.mpoly.numeric(unclass(x), indeterminate)
}





#' @export  
as.mpoly.numeric <- function(x, indeterminate = "x", ...){
  n <- length(x)
  
  ## make list and populate
  p <- list()
  p[[1]] <- c(coef = x[1])
  if(n > 1) for(deg in 1:(n-1)){
    v <- c(deg, x[deg+1])
    names(v) <- c(indeterminate, "coef")
    p[[deg+1]] <- v
  }
  
  ## clean out zeros
  p <- Filter(function(v) v[["coef"]] != 0, p)
  
  ## clean in case everything is removed
  if(length(p) == 0) p <- list(c(coef = 0))
  
  ## class and out
  class(p) <- "mpoly"
  p
}





























