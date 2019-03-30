#' Determine the variables in a mpoly object.
#'
#' Determine the variables in a mpoly object.
#'
#' @param p An mpoly or mpolyList object.
#' @return A character vector of the variable names.
#' @export
#' @examples
#' 
#' p <- mp("x + y^2")
#' vars(p)
#' 
#' p <- mp(c("x + y^2", "y - 2 x"))
#' vars(p)
#' 
vars <- function(p) {
  stopifnot(is.mpoly(p) || is.mpolyList(p))
  flat_list <- unlist(p)
  flat_list <- flat_list[names(flat_list) != "coef"]
  flat_list <- unique(names(flat_list))
  unique(gsub('\\^[0-9]?', '', flat_list))
}
