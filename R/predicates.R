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
  if (is_null(x)) FALSE else is.na(x)
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

#  FUNCTION: has_length --------------------------------------------------------
#
#' Check whether the variable has specified length
#'
#' @param x (any) The object to test
#'
#' @return TRUE if x is a valid URL, FALSE otherwise
#'
#' @examples
#' has_length(1, 1)
#' has_length(c("bob", "jane"), 2)
#' has_length(list(x = 1, y = 2, z = 3), 3)
#'
#' has_length(1:3, 1)
#' has_length(c("bob", "jane"), 3)
#' has_length(list(x = 1, y = 2, z = 3), 5)
#'
#' has_length(1:3, .min = 2)
#' has_length(1:3, .max = 10)
#'
#' has_length("A", .min = 2)
#' has_length(LETTERS, .max = 10)
#'
#' @export
#'
has_length <- function(x, .length, .min, .max) {
  (missing(.length) || length(x) == .length) &&
    (missing(.min) || length(x) >= .min) &&
    (missing(.max) || length(x) <= .max)
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
  if (is_character(x)) grepl("^(https|http)://", x) else FALSE
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
  if (is_character(x)) unname(fs::is_dir(x)) else FALSE
}

#  FUNCTION: is_file -----------------------------------------------------------
#
#' @rdname is_dir
#' @export
#'
is_file <- function(x) {
  if (is_character(x)) unname(fs::is_file(x)) else FALSE
}

#  FUNCTION: is_readable -------------------------------------------------------
#
#' @rdname is_dir
#' @export
#'
is_readable <- function(x) {
  if (is_character(x)) unname(fs::file_access(x, mode = "read")) else FALSE
}

#  FUNCTION: is_writeable ------------------------------------------------------
#
#' @rdname is_dir
#' @export
#'
is_writeable <- function(x) {
  if (is_character(x)) unname(fs::file_access(x, mode = "write")) else FALSE
}

#  FUNCTION: is_in -------------------------------------------------------------
#
#' Check whether elements of a variable are allowed (or not)
#'
#' `is_in()` checks whether the elements of `x` are in a set of allowed values.
#' `is_in_range()` checks whether the elements of `x` are within a numeric
#' range.
#' If `x` is a character vector each element is checked, returning a logical
#' vector of the same length.
#'
#' @param x (any) The object with elements to test
#' @param .values (any) The allowed (or not allowed) values.
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
#' is_in_range(1, .min = 0)
#' is_in_range(c(1.1, 2.2, 3.3), .min = 0, .max = 10)
#'
#' is_in_range(0, .min = 1)
#' is_in_range(c(1.1, 2.2, 3.3), .min = 2, .max = 3)
#'
#' @export
#'
is_in <- function(x, .values) {
  is_vector(x) & x %in% .values
}

#  FUNCTION: is_in_range -------------------------------------------------------
#
#' @param .min (numeric, optional) The minimum allowed value for a range. If not
#'   supplied, no minimum is set.
#' @param .max (numeric, optional) The maximum allowed value for a range. If not
#'   supplied, no maximum is set.
#'
#' @rdname is_in
#' @export
#'
is_in_range <- function(x, .min, .max) {
  is.numeric(x) &
    (if (missing(.min)) TRUE else x >= .min) &
    (if (missing(.max)) TRUE else x <= .max)
}

#  FUNCTION: has_names ---------------------------------------------------------
#
#' Checks whether the variable has names
#'
#' If `.names` is a character vector each element is checked, returning a
#' logical vector of the same length.
#'
#' @param x (any) The object to test
#' @param .names (character, optional) The names to check for. If not specified
#'   then the function checks for any names.
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
has_names <- function(x, .names) {
  names_exist <- !is_null(names(x))

  if (!missing(.names) && names_exist) {
    is_character(.names) || stop("'.names' must be a character vector")
    is_in(.names, names(x))
  } else {
    names_exist
  }
}

#  FUNCTION: has_char_length ---------------------------------------------------
#
#' Check whether the variable has specified character length
#'
#' If a character vector is supplied each element is checked, returning a
#' logical vector of the same length.
#'
#' @param x (any) The object to test
#'
#' @return TRUE if x is a valid URL, FALSE otherwise
#'
#' @examples
#' has_char_length("", 0)
#' has_char_length(c("some", "text"), 4)
#' has_char_length(c("some", "text"), .min = 1)
#' has_char_length(c("some", "text"), .max = 10)
#' has_char_length(c("some", "text"), .min = 1, .max = 10)
#'
#' has_char_length("sometext", 4)
#' has_char_length(c("different", "text"), .min = 5)
#' has_char_length(c("different", "text"), .max = 5)
#' has_char_length(c("different", "text"), .min = 5, .max = 8)
#'
#' @export
#'
has_char_length <- function(x, .length, .min, .max) {
  if (is_character(x)) {
    (if (missing(.length)) TRUE else nchar(x) == .length) &
      (if (missing(.min)) TRUE else nchar(x) >= .min) &
      (if (missing(.max)) TRUE else nchar(x) <= .max)
  } else {
    FALSE
  }
}
