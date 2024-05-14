#' @title Convenience function for creating request query
#'
#' @param x An orderable vector, i.e., those with relevant methods for `<=`, usually a date field.
#' @param lower Lower range bound of length 1. Will be coerced to character if not already.
#' @param upper Upper range bound of length 1. Will be coerced to character if not already.
#' @param inc.bounds `TRUE` (default) means inclusive bounds, i.e., `[lower,upper]`. `FALSE` means exclusive bounds, i.e., (lower,upper).
#'
#' @return  A list of maximum length 2, with expressions to use in later functions like `make_request`.
#' @export
#'
#' @examples
find_between <- function(x = "dateString", lower = NULL, upper = NULL, inc.bounds = TRUE) {
  
  if(inc.bounds == TRUE)
    bounds <- c("[$gte]", "[$lte]")
  else
    bounds <- c("[$gt]", "[$lt]")
  
  parms        <- list(as.character(lower), as.character(upper))
  names(parms) <- paste0("find[", x, "]", bounds)
  
  return(Filter(length, parms))
}