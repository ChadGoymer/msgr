#  FUNCTION: is_type -----------------------------------------------------------
#
#' Check the variable type & class
#'
#' This function returns `TRUE` if the variable type is correct and `FALSE`
#' otherwise. Depending on the type, a number of other attributes can be checked
#' simultaneously, such as length, using `n`, or size of rows and columns, using
#' `n_row` and `n_col` respectively.
#'
#' The following types can be checked:
#'
#' - `is_type()`: 'x"' must have type `type` and optionally length `n`.
#' - `is_class()`: 'x"' must have class `class` and optionally length `n`.
#' - `is_natural()`: 'x' must be a positive integer vector and optionally length
#'   `n`.
#' - `is_number()`: 'x' must be a numeric vector and optionally length `n`.
#' - `is_factor()`: 'x' must be a factor vector and optionally length `n`.
#' - `is_array()`: 'x' must be an array and optionally have dimension size
#'   `dims`.
#' - `is_matrix()`: 'x' must be a matrix and optionally have number of columns
#'   `n_col` and number of rows `n_row`.
#' - `is_data_frame()`: 'x' must be a data.frame and optionally have number of
#'   columns `n_col` and number of rows `n_row`.
#'
#' @param x (any) The object to test.
#' @param type (character) The allowed type.
#' @param n (integer, optional) The allowed length. If `NULL` the length is not
#'   checked. Default: `NULL`.
#'
#' @return TRUE if x is a valid URL, FALSE otherwise
#'
#' @seealso [rlang::is_null()], [rlang::is_atomic()], [rlang::is_vector()],
#'   [rlang::is_logical()], [rlang::is_integer()], [rlang::is_double()],
#'   [rlang::is_character()], [rlang::is_raw()], [rlang::is_bytes()],
#'   [rlang::is_list()]
#'
#' @examples
#'
#' # TRUE
#' is_type(1:3, "integer")
#' is_type(data.frame(a = 1:26, b = letters), "list")
#' is_type(1:3, "integer", n = 3)
#' # FALSE
#' is_type(data.frame(a = 1:26, b = letters), "data.frame")
#' is_type(1:3, "integer", n = 1)
#'
#' # TRUE
#' is_class(1:3, "integer")
#' is_class(data.frame(a = 1:26, b = letters), "data.frame")
#' is_class(1:3, "integer", n = 3)
#' # FALSE
#' is_class(data.frame(a = 1:26, b = letters), "list")
#' is_class(1:3, "integer", n = 1)
#'
#' # TRUE
#' is_natural(1:3)
#' is_natural(c(1.0, 2.0))
#' is_natural(1:3, n = 3)
#' # FALSE
#' is_natural(-1:-3)
#' is_natural(3.142)
#' is_natural(1:3, n = 1)
#'
#' # TRUE
#' is_number(1:3)
#' is_number(c(1.2, 2.4))
#' is_number(1:3, n = 3)
#' # FALSE
#' is_number("text")
#' is_number(1:3, n = 1)
#'
#' # TRUE
#' is_factor(factor(c("a", "b", "a")))
#' is_factor(factor(c("a", "b", "a")), n = 3)
#' is_factor(factor(c("a", "b", "a")), levels = c("a", "b"))
#' # FALSE
#' is_factor(1:3)
#' is_factor(factor(c("a", "b", "a")), n = 1)
#' is_factor(factor(c("a", "b", "a")), levels = c("A", "B"))
#'
#' # TRUE
#' is_array(array(1:3))
#' is_array(array(1:12, dim = c(3, 4)))
#' is_array(array(1:12, dim = c(3, 4)), dims = c(3, 4))
#' # FALSE
#' is_array(1:3)
#' is_array(array(1:12, dim = c(3, 4)), dims = c(4, 3))
#'
#' # TRUE
#' is_matrix(matrix(1:3))
#' is_matrix(matrix(1:12, nrow = 3, ncol = 4), n_col = 4)
#' is_matrix(matrix(1:12, nrow = 3, ncol = 4), n_row = 3)
#' # FALSE
#' is_matrix(1:3)
#' is_matrix(matrix(1:12, nrow = 3, ncol = 4), n_col = 3)
#' is_matrix(matrix(1:12, nrow = 3, ncol = 4), n_row = 4)
#'
#' # TRUE
#' is_data_frame(data.frame(a = 1:26, b = letters))
#' is_data_frame(data.frame(a = 1:26, b = letters), n_col = 2)
#' is_data_frame(data.frame(a = 1:26, b = letters), n_row = 26)
#' # FALSE
#' is_data_frame(list(a = 1:26, b = letters))
#' is_data_frame(data.frame(a = 1:26, b = letters), n_col = 5)
#' is_data_frame(data.frame(a = 1:26, b = letters), n_row = 5)
#'
#' @export
#'
is_type <- function(x, type, n = NULL) {
  is.character(type) && length(type) == 1 ||
    stop("'type' must be a character vector of length 1")
  typeof(x) == type && has_length(x = x, n = n)
}

#  FUNCTION: is_class ----------------------------------------------------------
#
#' @param class (character) The allowed class.
#' @rdname is_type
#' @export
#'
is_class <- function(x, class, n = NULL) {
  is.character(class) && length(class) == 1 ||
    stop("'class' must be a character vector of length 1")
  class %in% base::class(x) && has_length(x = x, n = n)
}

#  FUNCTION: is_natural --------------------------------------------------------
#
#' @rdname is_type
#' @export
#'
is_natural <- function(x, n = NULL) {
  is.numeric(x) && all(x == trunc(x) & x >= 0) && has_length(x = x, n = n)
}

#  FUNCTION: is_number ---------------------------------------------------------
#
#' @rdname is_type
#' @export
#'
is_number <- function(x, n = NULL) {
  is.numeric(x) && has_length(x = x, n = n)
}

#  FUNCTION: is_factor ---------------------------------------------------------
#
#' @param levels (character, optional) The allowed levels. If `NULL` the levels
#'   are not checked. Default: `NULL`.
#'
#' @rdname is_type
#' @export
#'
is_factor <- function(x, levels = NULL, n = NULL) {
  check_levels <- TRUE
  if (!is.null(levels)) {
    is.character(levels) ||
      stop("'levels' must be a character vector")
    check_levels <- all(base::levels(x) %in% levels)
  }

  is.factor(x) && has_length(x = x, n = n) && check_levels
}

#  FUNCTION: is_array ----------------------------------------------------------
#
#' @param dims (integer, optional) The allowed dimension sizes. If `NULL` the
#'   dimensions are not checked. Default: `NULL`.
#'
#' @rdname is_type
#' @export
#'
is_array <- function(x, dims = NULL) {
  check_dims <- TRUE
  if (!is.null(dims)) {
    is.numeric(dims) && all(dims == trunc(dims) & dims >= 0) ||
      stop("'dims' must be a positive integer vector")
    check_dims <- all(dim(x) == dims)
  }

  is.array(x) && check_dims
}

#  FUNCTION: is_matrix ---------------------------------------------------------
#
#' @param n_col (integer, optional) The allowed number of columns. If `NULL` the
#'   columns are not checked. Default: `NULL`.
#' @param n_row (integer, optional) The allowed number of rows. If `NULL` the
#'   rows are not checked. Default: `NULL`.
#'
#' @rdname is_type
#' @export
#'
is_matrix <- function(x, n_col = NULL, n_row = NULL) {
  check_n_col <- TRUE
  if (!is.null(n_col)) {
    is.numeric(n_col) && length(n_col) == 1 &&
      n_col == trunc(n_col) && n_col >= 0 ||
      stop("'n_col' must be a positive integer vector of length 1")
    check_n_col <- ncol(x) == n_col
  }

  check_n_row <- TRUE
  if (!is.null(n_row)) {
    is.numeric(n_row) && length(n_row) == 1 &&
      n_row == trunc(n_row) && n_row >= 0 ||
      stop("'n_row' must be a positive integer vector of length 1")
    check_n_row <- nrow(x) == n_row
  }

  is.matrix(x) && check_n_col && check_n_row
}

#  FUNCTION: is_data_frame -----------------------------------------------------
#
#' @rdname is_type
#' @export
#'
is_data_frame <- function(x, n_col = NULL, n_row = NULL) {
  check_n_col <- TRUE
  if (!is.null(n_col)) {
    is.numeric(n_col) && length(n_col) == 1 &&
      n_col == trunc(n_col) && n_col >= 0 ||
      stop("'n_col' must be a positive integer vector of length 1")
    check_n_col <- ncol(x) == n_col
  }

  check_n_row <- TRUE
  if (!is.null(n_row)) {
    is.numeric(n_row) && length(n_row) == 1 &&
      n_row == trunc(n_row) && n_row >= 0 ||
      stop("'n_row' must be a positive integer vector of length 1")
    check_n_row <- nrow(x) == n_row
  }

  is.data.frame(x) && check_n_col && check_n_row
}

#  FUNCTION: has_names ---------------------------------------------------------
#
#' Checks the variable structure
#'
#' This function returns `TRUE` if the variable structure is correct and `FALSE`
#' otherwise.
#'
#' The following structures can be checked:
#'
#' - `has_names()`: 'x' must have names and optionally have the specified
#'   `names`. A logical vector is returned checking each name.
#' - `has_length()`: 'x' must have valid length. You can specify the exact
#'   length using `n` or the minimum and/or maximum length using `n_min` and
#'   `n_max` respectively.
#'
#' @param x (any) The object to test
#' @param names (character, optional) The names to check for. If `NULL` the
#'   names are not checked. Default: `NULL`.
#'
#' @return TRUE if x has names, FALSE otherwise
#'
#' @seealso [rlang::is_empty()]
#'
#' @examples
#'
#' # TRUE
#' has_names(list(a = 1, b = 2))
#' has_names(list(a = 1, b = 2), "a")
#' has_names(list(a = 1, b = 2), c("a", "b"))
#' # FALSE
#' has_names(list(a = 1, b = 2), "c")
#' has_names(list(a = 1, b = 2), c("b", "c"))
#'
#' # TRUE
#' has_length(1, 1)
#' has_length(c("bob", "jane"), 2)
#' has_length(list(x = 1, y = 2, z = 3), 3)
#' has_length(1:3, n_min = 2)
#' has_length(1:3, n_max = 10)
#' # FALSE
#' has_length(1:3, 1)
#' has_length(c("bob", "jane"), 3)
#' has_length(list(x = 1, y = 2, z = 3), 5)
#' has_length("A", n_min = 2)
#' has_length(LETTERS, n_max = 10)
#'
#' @export
#'
has_names <- function(x, names = NULL) {
  names_exist <- !is.null(base::names(x))

  if (!is.null(names) && names_exist) {
    is.character(names) || stop("'names' must be a character vector")
    names %in% base::names(x)
  } else {
    names_exist
  }
}

#  FUNCTION: has_length --------------------------------------------------------
#
#' @param n (integer, optional) The allowed length. If `NULL` the exact length
#'   is not checked. Default: `NULL`.
#' @param n_min (integer, optional) The allowed minimum length. If `NULL` the
#'   minimum length is not checked. Default: `NULL`.
#' @param n_max (integer, optional) The allowed maximum length. If `NULL` the
#'   maximum length is not checked. Default: `NULL`.
#'
#' @rdname has_names
#' @export
#'
has_length <- function(x, n = NULL, n_min = NULL, n_max = NULL) {
  check_length <- TRUE
  if (!is.null(n)) {
    is.numeric(n) && length(n) == 1 && n == trunc(n) && n >= 0 ||
      stop("'n' must be a positive integer vector of length 1")
    check_length <- length(x) == n
  }

  check_min <- TRUE
  if (!is.null(n_min)) {
    is.numeric(n_min) && length(n_min) == 1 &&
      n_min == trunc(n_min) && n_min >= 0 ||
      stop("'n_min' must be a positive integer vector of length 1")
    check_min <- length(x) >= n_min
  }

  check_max <- TRUE
  if (!is.null(n_max)) {
    is.numeric(n_max) && length(n_max) == 1 &&
      n_max == trunc(n_max) && n_max >= 0 ||
      stop("'n_max' must be a positive integer vector of length 1")
    check_max <- length(x) <= n_max
  }

  check_length && check_min && check_max
}

#  FUNCTION: is_na -------------------------------------------------------------
#
#' Checks the variable values
#'
#' This function returns `TRUE` if the variable values are correct and `FALSE`
#' otherwise.
#'
#' The following values can be checked:
#'
#' - `is_na()`: Elements of 'x' must be `NA`.
#' - `is_in()`: Elements of 'x' must in specified `values`.
#' - `is_in_range()`: Elements of 'x' must be in the specified numeric range.
#' - `has_char_length`: Elements of 'x' must have valid character length. You
#'   can specify the exact length using `n` or the minimum and/or maximum length
#'   using `n_min` and `n_max` respectively.
#'
#' @param x (any) The object with elements to test
#'
#' @return TRUE if elements in x are in y, FALSE otherwise
#'
#' @examples
#'
#' # TRUE
#' is_na(NA)
#' is_na(c(NA, NA))
#' # FALSE
#' is_na(NULL)
#' is_na(1)
#' is_na("foo")
#' is_na(c(1, NA))
#'
#' # TRUE
#' is_in("a", letters)
#' is_in(c("a", "b", "c"), letters)
#' # FALSE
#' is_in(1, LETTERS)
#' is_in(1:2, LETTERS)
#'
#' # TRUE
#' is_in_range(1, min = 0)
#' is_in_range(c(1.1, 2.2, 3.3), min = 0, max = 10)
#' # FALSE
#' is_in_range(0, min = 1)
#' is_in_range(c(1.1, 2.2, 3.3), min = 2, max = 3)
#'
#' # TRUE
#' has_char_length("", 0)
#' has_char_length(c("some", "text"), 4)
#' has_char_length(c("some", "text"), n_min = 1)
#' has_char_length(c("some", "text"), n_max = 10)
#' has_char_length(c("some", "text"), n_min = 1, n_max = 10)
#' # FALSE
#' has_char_length("sometext", 4)
#' has_char_length(c("different", "text"), n_min = 5)
#' has_char_length(c("different", "text"), n_max = 5)
#' has_char_length(c("different", "text"), n_min = 5, n_max = 8)
#'
#' @export
#'
is_na <- function(x) {
  if (is.null(x)) FALSE else is.na(x)
}

#  FUNCTION: is_in -------------------------------------------------------------
#
#' @param values (any) The allowed (or not allowed) values.
#'
#' @rdname is_na
#' @export
#'
is_in <- function(x, values) {
  is_vector(x) & x %in% values
}

#  FUNCTION: is_in_range -------------------------------------------------------
#
#' @param min (numeric, optional) The minimum allowed value for a range. If
#'   `NULL` the minimum value is not checked. Default: `NULL`.
#' @param max (numeric, optional) The maximum allowed value for a range. If
#'   `NULL` the maximum value is not checked. Default: `NULL`.
#'
#' @rdname is_na
#' @export
#'
is_in_range <- function(x, min = NULL, max = NULL) {
  check_min <- TRUE
  if (!is.null(min)) {
    is.numeric(min) && length(min) == 1 && min >= 0 ||
      stop("'min' must be a positive numeric vector of length 1")
    check_min <- x >= min
  }

  check_max <- TRUE
  if (!is.null(max)) {
    is.numeric(max) && length(max) == 1 && max >= 0 ||
      stop("'max' must be a positive numeric vector of length 1")
    check_max <- x <= max
  }

  is.numeric(x) & check_min & check_max
}

#  FUNCTION: has_char_length ---------------------------------------------------
#
#' @param n (integer, optional) The allowed length. If `NULL` the exact
#'   character length is not checked. Default: `NULL`.
#' @param n_min (integer, optional) The allowed minimum length. If `NULL` the
#'   minimum character length is not checked. Default: `NULL`.
#' @param n_max (integer, optional) The allowed maximum length. If `NULL` the
#'   maximum character length is not checked. Default: `NULL`.
#'
#' @rdname is_na
#' @export
#'
has_char_length <- function(x, n = NULL, n_min = NULL, n_max = NULL) {
  if (is.character(x)) {
    check_length <- TRUE
    if (!is.null(n)) {
      is.numeric(n) && length(n) == 1 && n == trunc(n) && n >= 0 ||
        stop("'n' must be a positive integer vector of length 1")
      check_length <- nchar(x) == n
    }

    check_min <- TRUE
    if (!is.null(n_min)) {
      is.numeric(n_min) && length(n_min) == 1 &&
        n_min == trunc(n_min) && n_min >= 0 ||
        stop("'n_min' must be a positive integer vector of length 1")
      check_min <- nchar(x) >= n_min
    }

    check_max <- TRUE
    if (!is.null(n_max)) {
      is.numeric(n_max) && length(n_max) == 1 &&
        n_max == trunc(n_max) && n_max >= 0 ||
        stop("'n_max' must be a positive integer vector of length 1")
      check_max <- nchar(x) <= n_max
    }

    check_length & check_min & check_max
  } else {
    FALSE
  }
}

#  FUNCTION: is_file -----------------------------------------------------------
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
#'
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
is_file <- function(x) {
  if (is.character(x)) unname(fs::is_file(x)) else FALSE
}

#  FUNCTION: is_dir ------------------------------------------------------------
#
#' @rdname is_file
#' @export
#'
is_dir <- function(x) {
  if (is.character(x)) unname(fs::is_dir(x)) else FALSE
}

#  FUNCTION: is_readable -------------------------------------------------------
#
#' @rdname is_file
#' @export
#'
is_readable <- function(x) {
  if (is.character(x)) unname(fs::file_access(x, mode = "read")) else FALSE
}

#  FUNCTION: is_writeable ------------------------------------------------------
#
#' @rdname is_file
#' @export
#'
is_writeable <- function(x) {
  if (is.character(x)) unname(fs::file_access(x, mode = "write")) else FALSE
}

#  FUNCTION: is_url ------------------------------------------------------------
#
#' @rdname is_file
#' @export
#'
is_url <- function(x) {
  if (is.character(x)) grepl("^(https|http)://", x) else FALSE
}
