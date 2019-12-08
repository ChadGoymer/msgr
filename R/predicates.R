#  FUNCTION: is_na ----------------------------------------------------------------------------
#
#' Checks whether the variable is NA
#'
#' @param x (any) The object to test
#'
#' @return TRUE if x is NA, FALSE otherwise
#'
#' @examples
#' is_na(1)
#' is_na("foo")
#' is_na(NA)
#'
#' is_na(c(1, NA))
#' is_na(c(NA, NA))
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
#' @examples
#' is_url("http://something.com")
#' is_url("https://google.com")
#'
#' is_url(1)
#' is_url("foo")
#' is_url(NA)
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
#' @examples
#' is_dir(tempdir())
#' is_dir("/does/not/exist")
#' is_dir(1)
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
#' @examples
#' tmpfile <- tempfile()
#' file.create(tmpfile)
#'
#' is_file(tmpfile)
#'
#' is_file("/does/not/exist.txt")
#' is_file(1)
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
#' @examples
#' tmpfile <- tempfile()
#' file.create(tmpfile)
#'
#' is_readable(tmpfile)
#'
#' is_readable("/does/not/exist.txt")
#' is_readable(1)
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
#' @examples
#' tmpfile <- tempfile()
#' file.create(tmpfile)
#'
#' is_writeable(tmpfile)
#'
#' is_writeable("/does/not/exist.txt")
#' is_writeable(1)
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
#' @examples
#' is_in("a", letters)
#' is_in(c("a", "b", "c"), letters)
#'
#' is_in(1, LETTERS)
#' is_in(1:2, LETTERS)
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
#' @examples
#' x <- list(a = 1, b = 2)
#'
#' has_names(x, "a")
#' has_names(x, c("a", "b"))
#'
#' has_names(x, "c")
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
