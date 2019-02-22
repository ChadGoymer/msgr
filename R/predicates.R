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

#  FUNCTION: is_list --------------------------------------------------------------------------
#
#' Checks whether the variable is a list
#'
#' @param x (any) The object to test
#'
#' @return TRUE if x is a list, FALSE otherwise
#'
#' @export
#'
is_list <- function(x) {
  is.list(x)
}

#  FUNCTION: is_data_frame --------------------------------------------------------------------
#
#' Checks whether the variable is a data frame
#'
#' @param x (any) The object to test
#'
#' @return TRUE if x is a data frame, FALSE otherwise
#'
#' @export
#'
is_data_frame <- function(x) {
  is.data.frame(x)
}

#  FUNCTION: is_function ----------------------------------------------------------------------
#
#' Checks whether the variable is a function
#'
#' @param x (any) The object to test
#'
#' @return TRUE if x is a function, FALSE otherwise
#'
#' @export
#'
is_function <- function(x) {
  is.function(x)
}

#  FUNCTION: is_null --------------------------------------------------------------------------
#
#' Checks whether the variable is NULL
#'
#' @param x (any) The object to test
#'
#' @return TRUE if x is NULL, FALSE otherwise
#'
#' @export
#'
is_null <- function(x) {
  is.null(x)
}

#  FUNCTION: is_na ----------------------------------------------------------------------------
#
#' Checks whether the variable is NA
#'
#' @param x (any) The object to test
#'
#' @return TRUE if x is NA, FALSE otherwise
#'
#' @export
#'
is_na <- function(x) {
  !is.null(x) && is.na(x)
}

#  FUNCTION: is_url ---------------------------------------------------------------------------
#
#' Checks whether the variable is a valid URL
#'
#' @param x (any) The object to test
#'
#' @return TRUE if x is a valid URL, FALSE otherwise
#'
#' @export
#'
is_url <- function(x) {
  is_string(x) && grepl("^(https|http)://", x)
}

#  FUNCTION: is_dir ---------------------------------------------------------------------------
#
#' Checks whether the variable is a path to an existing directory
#'
#' @param x (any) The object to test
#'
#' @return TRUE if x is a path to an existing directory, FALSE otherwise
#'
#' @export
#'
is_dir <- function(x) {
  is_string(x) && dir.exists(x)
}

#  FUNCTION: is_file --------------------------------------------------------------------------
#
#' Checks whether the variable is a path to an existing file
#'
#' @param x (any) The object to test
#'
#' @return TRUE if x is a path to an existing file, FALSE otherwise
#'
#' @export
#'
is_file <- function(x) {
  is_string(x) && file.exists(x) && !file.info(x)$isdir
}

#  FUNCTION: is_readable ----------------------------------------------------------------------
#
#' Checks whether the variable is a path to an existing, readable file or directory
#'
#' @param x (any) The object to test
#'
#' @return TRUE if x is a path to an existing, readable file or directory, FALSE otherwise
#'
#' @export
#'
is_readable <- function(x) {
  is_string(x) && file.exists(x) && file.access(x, mode = 4)[[1]] == 0
}

#  FUNCTION: is_writeable ---------------------------------------------------------------------
#
#' Checks whether the variable is a path to an existing, writeable file or directory
#'
#' @param x (any) The object to test
#'
#' @return TRUE if x is a path to an existing, writeable file or directory, FALSE otherwise
#'
#' @export
#'
is_writeable <- function(x) {
  is_string(x) && file.exists(x) && file.access(x, mode = 2)[[1]] == 0
}

#  FUNCTION: is_in ----------------------------------------------------------------------------
#
#' Checks whether all elements of one variable are in another
#'
#' @param x (any) The object with elements to test
#' @param y (any) The object with elements to test against
#'
#' @return TRUE if all elements in x are in y, FALSE otherwise
#'
#' @export
#'
is_in <- function(x, y) {
  all(x %in% y)
}

#  FUNCTION: has_names ------------------------------------------------------------------------
#
#' Checks whether the variable has names
#'
#' @param x (any) The object to test
#' @param nm (character, optional) The names to check for. If not specified then the function
#'   checks for any names.
#'
#' @return TRUE if x has any names, FALSE otherwise
#'
#' @export
#'
has_names <- function(x, nm) {
  names_exist <- !is.null(names(x))

  if (!missing(nm) && names_exist) {
    is_character(nm) || stop("names ('nm') must be given as a character vector")
    is_in(nm, names(x))
  } else {
    names_exist
  }
}
