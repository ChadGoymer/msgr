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
  if (is_null(x)) {
    return(FALSE)
  }
  is.na(x)
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
  is_scalar_character(x) && grepl("^(https|http)://", x)
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
  is_scalar_character(x) && dir.exists(x)
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
  is_scalar_character(x) && file.exists(x) && !file.info(x)$isdir
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
  is_scalar_character(x) && file.exists(x) && file.access(x, mode = 4)[[1]] == 0
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
  is_scalar_character(x) && file.exists(x) && file.access(x, mode = 2)[[1]] == 0
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
  names_exist <- !is_null(names(x))

  if (!missing(nm) && names_exist) {
    is_character(nm) || stop("names ('nm') must be given as a character vector")
    is_in(nm, names(x))
  } else {
    names_exist
  }
}
