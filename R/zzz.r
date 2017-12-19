.onAttach <- function(...) {
  if(!interactive()) return()
  packageStartupMessage('  Please cite mpoly! See citation("mpoly") for details.')  
}