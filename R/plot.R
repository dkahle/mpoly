#' Plot the (real) variety of a polynomial
#'
#' The variety must have only 2 variables; it is plotted over a finite window;
#' and it will not discover zero-dimensional components.
#'
#' @param x an mpoly object
#' @param xlim,ylim numeric(2) vectors; x and y limits
#' @param varorder character(2); first element is x, second is y, defaults to
#'   \code{sort(vars(poly))}
#' @param add logical; should the plot be added to the current device?
#' @param n,nx,ny integer specifying number of points in the x and y dimensions
#' @param f argument to pass to [extendrange()]
#' @param col color of curve
#' @param ... arguments to pass to [contour()]
#' @usage \method{plot}{mpoly}(x, xlim, ylim, varorder, add = FALSE, n = 251, nx
#'   = n, ny = n, f = 0.05, col = "red", ...)
#' @exportS3Method base::plot
#' @return [NULL]
#' @examples
#'
#' p <- mp("x^2 + y^2 - 1")
#' plot(p, xlim = c(-1, 1), ylim = c(-1, 1))
#' plot(p, xlim = c(-1, 1), ylim = c(-1, 1), asp = 1)
#'
#' p <- mp("x^2 + 16 y^2 - 1")
#' plot(p, xlim = c(-1, 1), ylim = c(-1, 1))
#'
#' p <- mp("u^2 + 16 v^2 - 1")
#' plot(p, xlim = c(-1, 1), ylim = c(-1, 1))
#'
#' p <- mp("v^2 + 16 u^2 - 1")
#' plot(p, xlim = c(-1, 1), ylim = c(-1, 1))
#'
#' p <- mp("u^2 + 16 v^2 - 1")
#' plot(p, xlim = c(-1, 1), ylim = c(-1, 1), varorder = c("v","u"))
#'
#' p <- mp("y^2 - (x^3 + x^2)")
#' plot(p, xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5))
#'
#' plot(lissajous(3, 3, 0, 0), xlim = c(-1, 1), ylim = c(-1, 1), asp = 1)
#' plot(lissajous(5, 5, 0, 0), col = "steelblue", add = TRUE)
#' 
#' # how it works - inefficient
#' p <- lissajous(5, 5, 0, 0)
#' df <- expand.grid(
#'   x = seq(-1, 1, length.out = 26),
#'   y = seq(-1, 1, length.out = 26)
#' )
#' pf <- as.function(p)
#' df$z <- structure(pf(df), .Names = "p")
#' head(df)
#' with(df, plot(x, y, cex = .75, pch = 16, col = c("firebrick", "steelblue")[(z >= 0) + 1]))
#' plot(lissajous(5, 5, 0, 0), col = "black", add = TRUE)
#'
#' 
plot.mpoly <- function(x, xlim, ylim, varorder, add = FALSE, n = 251, nx = n, ny = n, f = 0.05, col = "red", ...) {
  
  if (is.character(x)) x <- mp(x)
  stopifnot( is.mpoly(x) )
  if (length(vars(x)) != 2) stop("`poly` must have exactly 2 variables.")
  if (missing(varorder)) varorder <- sort(vars(x))
  x <- reorder(x, varorder = varorder)
  
  if (isTRUE(add)) {
    if (missing(xlim)) xlim <- par("usr")[1:2] #|> extendrange(r = _, f = f)
    if (missing(ylim)) ylim <- par("usr")[3:4] #|> extendrange(r = _, f = f)
  } else {
    if (missing(xlim) || missing(ylim)) {
      stop("If `add = FALSE`, `xlim` and `ylim` must be specified.", call. = FALSE)
    }
  }
  
  xx <- seq(xlim[1], xlim[2], length.out = nx)
  yy <- seq(ylim[1], ylim[2], length.out = ny)

  df <- expand.grid(x = xx, y = yy)
  df$z <- as.function(x, varorder = varorder, silent = TRUE)(df)[,1]
  
  contour(
    xx, yy, matrix(df$z, nrow = nx), levels = 0, 
    drawlabels = FALSE, col = col, 
    xlab = varorder[1], ylab = varorder[2],  
    add = add,
    ...
  )
  
  
}
