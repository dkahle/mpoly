.onAttach <- function(...) {
  if(!interactive()) return()
  packageStartupMessage('  Please cite mpoly; see citation("mpoly") for details.')  
}