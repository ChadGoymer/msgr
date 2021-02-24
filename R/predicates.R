#  FUNCTION: is_na -------------------------------------------------------------
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

#  FUNCTION: is_integerish -----------------------------------------------------
#
#' Check the variable type
#'
#' @param x (any) The object to test
#'
#' @return TRUE if x is a valid URL, FALSE otherwise
#'
#' @examples
#' is_integerish(1:3)
#' is_integerish(c(1.0, 2.0))
#' is_integerish(3.142)
#'
#' is_scalar_integerish(1:3)
#' is_scalar_integerish(c(1.0, 2.0))
#' is_scalar_integerish(3.142)
#'
#' is_numeric(1:3)
#' is_numeric(c(1.2, 2.4))
#' is_numeric("text")
#'
#' is_scalar_numeric(1)
#' is_scalar_numeric(c(1.2, 2.4))
#' is_scalar_numeric("text")
#'
#' is_factor(factor(c("a", "b", "a")))
#' is_factor(1:3)
#'
#' is_data_frame(data.frame(a = 1:26, b = letters))
#' is_data_frame(list(a = 1:26, b = letters))
#'
#' @export
#'
is_integerish <- function(x) {
  rlang::is_integerish(x)
}

#  FUNCTION: is_scalar_integerish ----------------------------------------------
#
#' @rdname is_integerish
#' @export
#'
is_scalar_integerish <- function(x) {
  rlang::is_scalar_integerish(x)
}

#  FUNCTION: is_numeric --------------------------------------------------------
#
#' @rdname is_integerish
#' @export
#'
is_numeric <- function(x) {
  is.numeric(x)
}

#  FUNCTION: is_scalar_numeric -------------------------------------------------
#
#' @rdname is_integerish
#' @export
#'
is_scalar_numeric <- function(x) {
  is.numeric(x) && is_scalar_atomic(x)
}

#  FUNCTION: is_factor ---------------------------------------------------------
#
#' @rdname is_integerish
#' @export
#'
is_factor <- function(x) {
  is.factor(x)
}

#  FUNCTION: is_data_frame -----------------------------------------------------
#
#' @rdname is_integerish
#' @export
#'
is_data_frame <- function(x) {
  is.data.frame(x)
}

#  FUNCTION: is_url ------------------------------------------------------------
#
#' Checks whether the variable is a valid URL
#'
#' If a character vector is supplied each element is checked, returning a
#' logical vector of the same length.
#'
#' @param x (any) The object to test
#'
#' @return TRUE if x is a valid URL, FALSE otherwise
#'
#' @examples
#' is_url("http://something.com")
#' is_url("https://google.com")
#' is_url(c("http://something.com", "https://google.com"))
#'
#' is_url(1)
#' is_url("foo")
#' is_url(NA)
#'
#' @export
#'
is_url <- function(x) {
  if (is_character(x)) {
    grepl("^(https|http)://", x)
  } else {
    FALSE
  }
}

#  FUNCTION: is_dir ------------------------------------------------------------
#
#' Check file paths and directories
#'
#' These functions check whether the directories or files exist and whether the
#' current user has read or write permissions. If a character vector is supplied
#' each element is checked, returning a logical vector of the same length.
#'
#' @param x (any) The object to test
#'
#' @return TRUE if x is a path to an existing directory, FALSE otherwise
#'
#' @examples
#' is_dir(tempdir())
#' is_dir("/does/not/exist")
#'
#' tmpfile <- tempfile()
#' file.create(tmpfile)
#'
#' is_file(tmpfile)
#' is_file("/does/not/exist.txt")
#'
#' is_readable(tmpfile)
#' is_readable("/does/not/exist.txt")
#'
#' is_writeable(tmpfile)
#' is_writeable("/does/not/exist.txt")
#'
#' @export
#'
is_dir <- function(x) {
  if (is_character(x)) {
    unname(fs::is_dir(x))
  } else {
    FALSE
  }
}

#  FUNCTION: is_file -----------------------------------------------------------
#
#' @rdname is_dir
#' @export
#'
is_file <- function(x) {
  if (is_character(x)) {
    unname(fs::is_file(x))
  } else {
    FALSE
  }
}

#  FUNCTION: is_readable -------------------------------------------------------
#
#' @rdname is_dir
#' @export
#'
is_readable <- function(x) {
  if (is_character(x)) {
    unname(fs::file_access(x, mode = "read"))
  } else {
    FALSE
  }
}

#  FUNCTION: is_writeable ------------------------------------------------------
#
#' @rdname is_dir
#' @export
#'
is_writeable <- function(x) {
  if (is_character(x)) {
    unname(fs::file_access(x, mode = "write"))
  } else {
    FALSE
  }
}

#  FUNCTION: is_in -------------------------------------------------------------
#
#' Checks whether elements of one variable are in another
#'
#' If `x` is a character vector each element is checked, returning a logical
#' vector of the same length.
#'
#' @param x (any) The object with elements to test
#' @param y (any) The object with elements to test against
#'
#' @return TRUE if elements in x are in y, FALSE otherwise
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
  x %in% y
}

#  FUNCTION: has_names ---------------------------------------------------------
#
#' Checks whether the variable has names
#'
#' If `nm` is a character vector each element is checked, returning a logical
#' vector of the same length.
#'
#' @param x (any) The object to test
#' @param nm (character, optional) The names to check for. If not specified then
#'   the function checks for any names.
#'
#' @return TRUE if x has names, FALSE otherwise
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
