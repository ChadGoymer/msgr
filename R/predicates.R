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

#  FUNCTION: is_scalar ------------------------------------------------------------------------
#
#' Checks whether the variable is an atomic vector of length 1
#'
#' @param x (any) The object to test
#'
#' @return TRUE if x is an atomic vector of length 1, FALSE otherwise
#'
#' @export
#'
is_scalar <- function(x) {
  is_vector(x) && identical(length(x), 1L)
}

#  FUNCTION: is_logical -----------------------------------------------------------------------
#
#' Checks whether the variable is a logical vector
#'
#' @param x (any) The object to test
#'
#' @return TRUE if x is a logical vector, FALSE otherwise
#'
#' @export
#'
is_logical <- function(x) {
  is_vector(x) && is.logical(x)
}

#  FUNCTION: is_boolean -----------------------------------------------------------------------
#
#' Checks whether the variable is a logical vector of length 1
#'
#' @param x (any) The object to test
#'
#' @return TRUE if x is a logical vector of length 1, FALSE otherwise
#'
#' @export
#'
is_boolean <- function(x) {
  is_logical(x) && is_scalar(x)
}

#  FUNCTION: is_numeric -----------------------------------------------------------------------
#
#' Checks whether the variable is a numeric vector
#'
#' @param x (any) The object to test
#'
#' @return TRUE if x is a numeric vector, FALSE otherwise
#'
#' @export
#'
is_numeric <- function(x) {
  is_vector(x) && is.numeric(x)
}

#  FUNCTION: is_number ------------------------------------------------------------------------
#
#' Checks whether the variable is a numeric vector of length 1
#'
#' @param x (any) The object to test
#'
#' @return TRUE if x is a numeric vector of length 1, FALSE otherwise
#'
#' @export
#'
is_number <- function(x) {
  is_numeric(x) && is_scalar(x)
}

#  FUNCTION: is_integer -----------------------------------------------------------------------
#
#' Checks whether the variable is a numeric vector of integers
#'
#' @param x (any) The object to test
#'
#' @return TRUE if x is a numeric vector of integers, FALSE otherwise
#'
#' @export
#'
is_integer <- function(x) {
  is_numeric(x) && all(x == as.integer(x), na.rm = TRUE)
}

#  FUNCTION: is_natural -----------------------------------------------------------------------
#
#' Checks whether the variable is a natural number
#'
#' @param x (any) The object to test
#'
#' @return TRUE if x is a natural number, FALSE otherwise
#'
#' @export
#'
is_natural <- function(x) {
  is_integer(x) && is_scalar(x) && isTRUE(x > 0)
}

#  FUNCTION: is_character ---------------------------------------------------------------------
#
#' Checks whether the variable is a character vector
#'
#' @param x (any) The object to test
#'
#' @return TRUE if x is a character vector, FALSE otherwise
#'
#' @export
#'
is_character <- function(x) {
  is_vector(x) && is.character(x)
}

#  FUNCTION: is_string ------------------------------------------------------------------------
#
#' Checks whether the variable is a character vector of length 1
#'
#' @param x (any) The object to test
#'
#' @return TRUE if x is a character vector of length 1, FALSE otherwise
#'
#' @export
#'
is_string <- function(x) {
  is_character(x) && is_scalar(x)
}
