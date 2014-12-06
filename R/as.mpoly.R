#' Convert an object to an mpoly
#'
#' mpoly is the most basic function used to create objects of class mpoly. 
#' 
#' @param x an object of class lm.
#' @return the object formated as a mpoly object.
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @seealso \code{\link{mp}}
#' @export as.mpoly
#' @S3method as.mpoly default
#' @S3method as.mpoly lm
#' @examples
#' \dontrun{
#' library(plyr)
#' library(ggplot2)
#' library(stringr)
#' n <- 101
#' s <- seq(-5, 5, length.out = n)
#' 
#' # one dimensional case
#' df <- data.frame(x = s)
#' df <- mutate(df, y = -x^2 + 2*x - 3 + rnorm(n, 0, 2))
#' qplot(x, y, data = df)
#' mod <- lm(y ~ x + I(x^2), data = df)
#' p <- as.mpoly(mod)
#' f <- as.function(p)
#' qplot(x, y, data = df) +
#'   stat_function(fun = f, colour = 'red')
#' 
#'
#' # two dimensional case
#' df <- expand.grid(x = s, y = s)
#' df <- mutate(df, z = x^2 - y^2 + 2 * x*y + rnorm(n^2, 0, 3))
#' qplot(x, y, data = df, geom = 'raster', fill = z)
#' mod <- lm(z ~ x + y + I(x^2) + I(y^2) + I(x*y), data = df)
#' p <- as.mpoly(mod)
#' f <- as.function(p)
#' df$fit <- apply(df[,c('x','y')], 1, f)
#' qplot(x, y, data = df, geom = 'raster', fill = fit)
#' qplot(x, y, data = df, geom = 'raster', fill = z - fit) # residuals
#' 
#' # to do
#' mod <- lm(z ~ poly(x, y, degree = 2), data = df)
#' 
#' }
#' 
as.mpoly <- function(x) UseMethod('as.mpoly')

as.mpoly.default <- function(x) 
  stop('object not supported.  see ?as.mpoly for details.')

as.mpoly.lm <- function(x){
  coefs <- coefficients(x)
  coef_names <- names(coefs)
  coef_names[coef_names == '(Intercept)'] <- 1
  I_ndcs <- which(str_detect(coef_names, 'I([0-9a-zA-Z]*)'))
  if(length(I_ndcs) > 0){
    coef_names[I_ndcs] <- sapply(as.list(coef_names[I_ndcs]), 
      function(s) {
          str_sub(s, 3, -2)
      }
    )
  }
  coef_names  <- str_replace_all(coef_names, ' \\* ', ' ')
  mp_str <- paste(coefs, coef_names, sep = ' ', collapse = ' + ')
  mp(mp_str)
}


