#  FUNCTION: is_vector ------------------------------------------------------------------------
#
#' Checks whether the variable is an atomic vector
#'
#' @param x (any) The object to test
#'
#' @return TRUE if x is an atomic vector, FALSE otherwise
#'
#' @export
#'
is_vector <- function(x) {
  !is.null(x) && is.atomic(x) && !is.array(x)
}
