#' Bezier polynomials
#' 
#' Compute the Bezier polynomials of a given collection of points.  Note that 
#' evaluating the polynomials with \code{\link{as.function.mpoly}} will not take
#' advantage of efficient algorithms such as De Casteljau's algorithm to do so. 
#' For that, see the bezier package.
#' 
#' @param ... either a sequence of points or a matrix/data frame of points, see examples
#' @param indeterminate the indeterminate of the resulting polynomial
#' @return a mpoly object
#' @author David Kahle
#' @export
#' @examples
#' 
#' p1 <- c(0,  0)
#' p2 <- c(1,  1)
#' p3 <- c(2, -1)
#' p4 <- c(3,  0)
#' bezier(p1, p2, p3, p4)
#' 
#' 
#' points <- data.frame(x = 0:3, y = c(0,1,-1,0))
#' bezier(points)
#' 
#' 
#' points <- data.frame(x = 0:2, y = c(0,1,0))
#' bezier(points)
#' 
#' 
#' 
#' \dontrun{  # visualize the bernstein polynomials
#' 
#' library(ggplot2); theme_set(theme_bw())
#'
#' s <- seq(0, 1, length.out = 101) 
#' 
#' ## example 1
#' points <- data.frame(x = 0:3, y = c(0,1,-1,0))
#' (bezPolys <- bezier(points))
#' 
#' df <- t(sapply(s, as.function(bezPolys)) )
#' df <- as.data.frame(df)
#' names(df) <- c("x", "y")
#' qplot(x, y, data = df, geom = "path") +
#'   geom_path(data = points, color = "red") +
#'   geom_point(data = points, color = "red", size = 8)
#'   
#'   
#'   
#'   
#' ## example 2
#' points <- data.frame(x = 0:2, y = c(0,1,0))
#' (bezPolys <- bezier(points))
#' 
#' df <- t(sapply(s, as.function(bezPolys)) )
#' df <- as.data.frame(df)
#' names(df) <- c("x", "y")
#' qplot(x, y, data = df, geom = "path") +
#'   geom_path(data = points, color = "red") +
#'   geom_point(data = points, color = "red", size = 8)
#' 
#' 
#' 
#' 
#' ## example 3
#' points <- data.frame(x = c(-1,-2,2,1), y = c(0,1,1,0))
#' (bezPolys <- bezier(points))
#' 
#' df <- t(sapply(s, as.function(bezPolys)) )
#' df <- as.data.frame(df)
#' names(df) <- c("x", "y")
#' qplot(x, y, data = df, geom = "path") +
#'   geom_path(data = points, color = "red") +
#'   geom_point(data = points, color = "red", size = 8)
#'   
#'   
#'   
#' ## example 4
#' points <- data.frame(x = c(-1,2,-2,1), y = c(0,1,1,0))
#' (bezPolys <- bezier(points))
#' 
#' df <- t(sapply(s, as.function(bezPolys)) )
#' df <- as.data.frame(df)
#' names(df) <- c("x", "y")
#' qplot(x, y, data = df, geom = "path") +
#'   geom_path(data = points, color = "red") +
#'   geom_point(data = points, color = "red", size = 8)
#' 
#' 
#' 
#' 
#' }
#' 
bezier <- function(..., indeterminate = "t"){  
  
  ## grab input
  dots <- as.list(match.call(expand.dots = FALSE))$"..."
  names(dots) <- as.character(match.call(expand.dots = FALSE)$"...")
  dots <- lapply(dots, eval)
  
  ## parse input into a data frame
  if(length(dots) == 1 && (is.data.frame(dots[[1]]) || is.matrix(dots[[1]]))){    
    points <- dots[[1]]
    if(is.matrix(points)) points <- as.data.frame(points)
    if(is.null(names(points))) names(points) <- paste0("x", 1:ncol(points))
  } else if(
    all(vapply(dots, is.numeric, logical(1))) &&
    all(vapply(dots, length, numeric(1)) == length(dots[[1]]))
  ){
    points <- as.data.frame(do.call(rbind, dots))    
    if(is.null(names(dots[[1]]))){
      names(points) <- paste0("x", 1:ncol(points))
    } else {
      names(points) <- names(dots[[1]])
    }
    row.names(points) <- NULL
  }
  

  ## make polynomial
  n <- nrow(points) 
  bernPolys <- bernstein(0:(n-1), n-1, "t")
  
  ## initialize bezPoly
  d <- ncol(points)
  bezPoly <- mp(rep("0", d))
  
  for(k in 1:n){
    ## convert vector of numerics into mpolyList
    polyWeights <- lapply(unname(points[k,]), function(.) mpoly(list(c(coef = .))))
    class(polyWeights) <- "mpolyList"
    
    ## duplicate the bern basis function to the same length
    basis <- replicate(d, bernPolys[[k]], simplify = FALSE)
    class(basis) <- "mpolyList"
    
    ## accumulate onto bezPoly
    bezPoly <- bezPoly + polyWeights * basis
  }
  
  ## return
  bezPoly
}

