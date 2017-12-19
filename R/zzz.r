.onAttach <- function(...) {
  if(!interactive()) return()
  packageStartupMessage('Please cite mpoly if you use it; type citation("mpoly") for details.')  
}