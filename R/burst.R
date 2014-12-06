#' Enumerate integer r-vectors summing to n
#'
#' Determine all r-vectors with nonnegative integer entries summing to n
#' 
#' @param n integer to sum to
#' @param r number of cells (dimension of vectors)
#' @return a matrix whose rows are the n-tuples
#' @export
#' @examples
#' burst(4)
#' apply(burst(4), 1, sum) # all sum to 4
#' 
#' burst(4, 4)
#' burst(4, 3)
#' burst(4, 2)
#' burst(4, 1)
#' apply(burst(4,3), 1, sum) # all sum to 4
#' apply(burst(4,2), 1, sum) # all sum to 4
#' apply(burst(4,1), 1, sum) # all sum to 4
#'
#' burst(10, 4) # all possible 2x2 contingency tables with n=10
#' burst(10, 4) / 10 # all possible empirical relative frequencies
#'
burst <- function(n, r = n){
  stopifnot(is.wholenumber(n))
  stopifnot(n > 0)
  stopifnot(is.wholenumber(r))
  stopifnot(r > 0)
  
  
  
  
  
  
  
list_to_dataframe <- function(res, labels = NULL, id_name = NULL, id_as_factor = FALSE) {
	
  make_names <- NULL; rm(make_names)
	
  null <- vapply(res, is.null, logical(1))
  res <- res[!null]
  if (length(res) == 0) return(data.frame())

  if (!is.null(labels)) {
    stopifnot(nrow(labels) == length(null))
    labels <- labels[!null, , drop = FALSE]
  }
  names_res <- names(res)
  if (!is.null(id_name) && is.null(labels) && !is.null(names_res)) {
    stopifnot(length(id_name) == 1)
    if (id_as_factor)
      names_res <- factor(names_res, levels = unique(names_res))
    labels <- data.frame(.id = names_res, stringsAsFactors = FALSE)
    names(labels) <- id_name
  }

  # Figure out how to turn elements into a data frame
  atomic <- unlist(lapply(res, is.atomic))
  df <- unlist(lapply(res, is.data.frame))
  mat <- unlist(lapply(res, is.matrix))

  if (all(mat)) {
    resdf <- as.data.frame(rbind.fill.matrix(res))
    rows <- unlist(lapply(res, NROW))
  } else if (all(atomic)) {
    nrow <- length(res)
    ncol <- unique(unlist(lapply(res, length)))
    if (length(ncol) != 1) stop("Results do not have equal lengths")

    vec <- unname(do.call("c", res))

    resdf <- quickdf(unname(split(vec, rep(seq_len(ncol), nrow))))
    names(resdf) <- make_names(res[[1]], "V")

    rows <- rep(1, length(nrow))
  } else if (all(df)) {
    resdf <- rbind.fill(res)
    rows <- unlist(lapply(res, NROW))
  } else {
    stop("Results must be all atomic, or all data frames")
  }

  if(is.null(labels)) return(unrowname(resdf))

  # Add labels to results
  names(labels) <- make_names(labels, "X")

  cols <- setdiff(names(labels), names(resdf))
  labels <- labels[rep(1:nrow(labels), rows), cols, drop = FALSE]

  unrowname(cbind(labels, resdf))
}  
  
  
  
  
  
  
  
  
  
  
  
  
  parts <- partitions(n)

  partsWOzeros <- apply(parts, 1, function(row) row[row != 0] )
  
  partsWOzeros <- partsWOzeros[sapply(partsWOzeros, length) <= r]
  rvectors <- lapply(partsWOzeros, function(v){
    if(length(v) < r) return(c(v, rep(0, r - length(v))))
    v
  })
  
  unname(as.matrix(list_to_dataframe(
    lapply(rvectors, function(x) as.data.frame(permutations(x)))
  )))
}