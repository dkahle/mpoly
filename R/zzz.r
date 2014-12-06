.onAttach <- function(...) {
  if(!interactive()) return()
  packageStartupMessage('please cite mpoly if you use it.')  
  packageStartupMessage('  type citation("mpoly") for details how.')    
}