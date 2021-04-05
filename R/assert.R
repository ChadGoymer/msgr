#  FUNCTION: assert ------------------------------------------------------------
#
#' Display an error if a condition is not met
#'
#' This function calls the [error()] function to display an error if the
#' specified condition is false. If a message is not specified then a generic
#' message is displayed.
#'
#' @param condition (boolean) The condition to check.
#' @param ... (character) message to be displayed or written to file.
#' @param level (integer, optional) The level of the message, from 1 to 10.
#'   Default: 1.
#' @param msg_level (integer, optional) The maximum level of messages to output.
#'   Default: set in the option `"msgr.level"`.
#' @param msg_types (character, optional) The type to write or display. Must
#'   either NULL or one or more from "INFO", "WARNING" or "ERROR". Default: set
#'   in the option `"msgr.types"`.
#' @param log_path (character, optional) The file path to the text log file. If
#'   set to "", then no logs are written. Default: set in the option
#'   `"msgr.log_path"`.
#'
#' @return If assertion passes then `TRUE` is returned. This allows you to make
#'   multiple assertions separated by `&`.
#'
#' @examples \dontrun{
#'
#'   # Use assert() to create conditional timed errors
#'   x <- 1
#'   assert(x > 0, "Condition is true so this error is not shown")
#'   assert(x < 0, "Condition is false so this error is shown")
#'
#'   # As with error() a level can be set
#'   assert(x < 0, "This level 2 error is not shown by default", level = 2)
#'
#'   # Set default level in options to determine what is shown
#'   options(msgr.level = 2)
#'   assert(x < 0, "This is a level 2 error, so is shown now", level = 2)
#'
#' }
#'
#' @export
#'
assert <- function(
  condition,
  ...,
  level     = 1,
  msg_level = getOption("msgr.level"),
  msg_types = getOption("msgr.types"),
  log_path  = getOption("msgr.log_path")
) {
  is_natural(level, n = 1) && is_in_range(level, min = 1, max = 10) ||
    stop("'level' must be an integer between 1 and 10: ", level)
  is_natural(msg_level, n = 1) && is_in_range(msg_level, min = 1, max = 10) ||
    stop("'msg_level' must be an integer between 1 and 10: ", msg_level)
  is.null(msg_types) || is.character(msg_types) ||
    stop("'msg_types' must be NULL or a character vector: ", msg_types)
  all(is_in(msg_types, c("INFO", "WARNING", "ERROR"))) ||
    stop("'msg_types' must be either 'INFO', 'WARNING' or 'ERROR': ", msg_types)
  is.character(log_path) && length(log_path) == 1 ||
    stop("'log_path' must be a string: ", log_path)

  if (!isTRUE(condition)) {
    uneval_condition <- substitute(condition)

    prefix <- ""
    if (sys.nframe() > 1) {
      calling_function <- deparse(sys.calls()[[sys.nframe() - 1]][[1]])
      prefix <- paste0("In ", calling_function, "(): ")
    }

    msg <- list(...)
    if (length(msg) == 0) {
      msg <- paste(deparse(uneval_condition), "is false")
    } else {
      msg <- paste(as.character(msg), collapse = "")
    }

    error(
      prefix, msg,
      level     = level,
      msg_level = msg_level,
      msg_types = msg_types,
      log_path  = log_path
    )
  }

  invisible(TRUE)
}

#  FUNCTION: assert_type -------------------------------------------------------
#
#' Display an error if not the correct type
#'
#' This function calls the [error()] function to display an error if the
#' variable type of the input is not correct. Depending on the type, a number of
#' other attributes can be checked simultaneously, such as length, using `n`, or
#' size of rows and columns, using `n_row` and `n_col` respectively.
#'
#' The following types can be checked:
#'
#' - `assert_type()`: 'x"' must have type `type` and optionally length `n`.
#' - `assert_class()`: 'x"' must have class `class` and optionally length `n`.
#' - `assert_null()`: 'x' must be `NULL`
#' - `assert_atomic()`: 'x' must be an atomic vector and optionally length `n`.
#' - `assert_vector()`: 'x' must be a vector and optionally length `n`.
#' - `assert_logical()`: 'x' must be a logical vector and optionally length `n`.
#' - `assert_integer()`: 'x' must be an integer vector and optionally length
#'   `n`.
#' - `assert_natural()`: 'x' must be a positive integer vector and optionally
#'   length `n`.
#' - `assert_double()`: 'x' must be a double vector and optionally length `n`.
#' - `assert_number()`: 'x' must be a numeric vector and optionally length `n`.
#' - `assert_character()`: 'x' must be a character vector and optionally length
#'   `n`.
#' - `assert_factor()`: 'x' must be a factor vector and optionally length `n`.
#' - `assert_list()`: 'x' must be a list and optionally length `n`.
#' - `assert_array()`: 'x' must be an array and optionally have dimension sizes
#'   `dims`.
#' - `assert_matrix()`: 'x' must be a matrix and optionally have number of
#'   columns `n_col` and number of rows `n_row`.
#' - `assert_data_frame()`: 'x' must be a data.frame and optionally have number
#'   of columns `n_col` and number of rows `n_row`.
#' - `assert_function()`: 'x' must be a function
#' - `assert_formula()`: 'x' must be a formula
#'
#' @param x (any) The object to test.
#' @param type (character) The allowed type.
#' @param n (integer, optional) The allowed length. If `NULL` the length is not
#'   checked. Default: `NULL`.
#' @param level (integer, optional) The level of the message, from 1 to 10.
#'   Default: 1.
#' @param msg_level (integer, optional) The maximum level of messages to output.
#'   Default: set in the option `"msgr.level"`.
#' @param msg_types (character, optional) The type to write or display. Must
#'   either NULL or one or more from "INFO", "WARNING" or "ERROR". Default: set
#'   in the option `"msgr.types"`.
#' @param log_path (character, optional) The file path to the text log file. If
#'   set to "", then no logs are written. Default: set in the option
#'   `"msgr.log_path"`.
#'
#' @return If assertion passes then `TRUE` is returned. This allows you to make
#'   multiple assertions separated by `&`.
#'
#' @examples \dontrun{
#'
#' # No error
#' assert_type(data.frame(x = 1:3), "list")
#' assert_type(list(1, 2, 3), "list", n = 3)
#' # Error
#' assert_type(1:3, "list")
#' assert_type(1:3, "integer", n = 1)
#'
#' # No error
#' assert_class(data.frame(x = 1:3), "data.frame")
#' assert_class(list(1, 2, 3), "list", n = 3)
#' # Error
#' assert_class(data.frame(x = 1:3), "list")
#' assert_class(1:3, "integer", n = 1)
#'
#' # No error
#' assert_null(NULL)
#' # Error
#' assert_null(1)
#'
#' # No error
#' assert_atomic(1:3)
#' assert_atomic(1:3, n = 3)
#' # Error
#' assert_atomic(list(1))
#' assert_atomic(1:3, n = 1)
#'
#' # No error
#' assert_vector(1:3)
#' assert_vector(1:3, n = 3)
#' # Error
#' assert_vector(options)
#' assert_vector(1:3, n = 1)
#'
#' # No error
#' assert_logical(TRUE)
#' assert_logical(c(TRUE, FALSE, TRUE), n = 3)
#' # Error
#' assert_logical(1)
#' assert_logical(c(TRUE, FALSE, TRUE), n = 1)
#'
#' # No error
#' assert_integer(1:3)
#' assert_integer(1:3, n = 3)
#' # Error
#' assert_integer(c(1.414, 3.142))
#' assert_integer(1:3, n = 1)
#'
#' # No error
#' assert_natural(c(1, 2, 3))
#' assert_natural(1:3, n = 3)
#' # Error
#' assert_natural(-1:3)
#' assert_natural(c(1.414, 3.142))
#' assert_natural(1:3, n = 1)
#'
#' # No error
#' assert_double(c(1.414, 1.732, 2.000))
#' assert_double(c(1.414, 1.732, 2.000), n = 3)
#' # Error
#' assert_double(c("A", "B", "C"))
#' assert_double(c(1.414, 1.732, 2.000), n = 1)
#'
#' # No error
#' assert_number(c(1.414, 1.732, 2.000))
#' assert_number(c(1.414, 1.732, 2.000), n = 3)
#' # Error
#' assert_number(c("A", "B", "C"))
#' assert_number(c(1.414, 1.732, 2.000), n = 1)
#'
#' # No error
#' assert_character(c("A", "B", "C"))
#' assert_character(c("A", "B", "C"), n = 3)
#' # Error
#' assert_character(1:3)
#' assert_character(c("A", "B", "C"), n = 1)
#'
#' # No error
#' assert_factor(factor(c("A", "B", "A")))
#' assert_factor(factor(c("A", "B", "A")), n = 3)
#' assert_factor(factor(c("A", "B", "A")), levels = c("A", "B"))
#' # Error
#' assert_factor(1:3)
#' assert_factor(factor(c("A", "B", "A")), n = 1)
#' assert_factor(factor(c("A", "B", "A")), levels = c("B", "C"))
#'
#' # No error
#' assert_list(list(1, 2, 3))
#' assert_list(list(1, 2, 3), n = 3)
#' # Error
#' assert_list(c(1, 2, 3))
#' assert_list(list(1, 2, 3), n = 1)
#'
#' # No error
#' assert_data_frame(data.frame(x = 1:26, y = LETTERS))
#' assert_data_frame(data.frame(x = 1:26, y = LETTERS), n_col = 2)
#' assert_data_frame(data.frame(x = 1:26, y = LETTERS), n_row = 26)
#' assert_data_frame(data.frame(x = 1:26, y = LETTERS), n_col = 2, n_row = 26)
#' # Error
#' assert_data_frame(list(x = 1:26, y = LETTERS))
#' assert_data_frame(data.frame(x = 1:26, y = LETTERS), n_col = 1)
#' assert_data_frame(data.frame(x = 1:26, y = LETTERS), n_row = 10)
#' assert_data_frame(data.frame(x = 1:26, y = LETTERS), n_col = 1, n_row = 10)
#'
#' # No error
#' assert_function(options)
#' # Error
#' assert_function(1:3)
#'
#' # No error
#' assert_formula(x ~ 1)
#' # Error
#' assert_formula(x == 1)
#'
#' }
#'
#' @export
#'
assert_type <- function(
  x,
  type,
  n         = NULL,
  level     = 1,
  msg_level = getOption("msgr.level"),
  msg_types = getOption("msgr.types"),
  log_path  = getOption("msgr.log_path")
) {
  types <- c(
    "logical", "integer", "double", "complex", "character", "raw", "list",
    "NULL", "closure", "special", "builtin", "environment", "S4", "symbol",
    "pairlist", "promise", "language", "char", "...", "any", "expression",
    "externalptr", "bytecode", "weakref"
  )
  is.character(type) && length(type) == 1 ||
    stop("'type' must be a character vector of length 1")
  all(is_in(type, types)) ||
    stop("'type' must be either '", paste(types, collapse = "', '"), "'")
  is.null(n) || is_natural(n, n = 1) ||
    stop("'n' must be a positive integer")
  is_natural(level, n = 1) && is_in_range(level, min = 1, max = 10) ||
    stop("'level' must be an integer between 1 and 10: ", level)
  is_natural(msg_level, n = 1) && is_in_range(msg_level, min = 1, max = 10) ||
    stop("'msg_level' must be an integer between 1 and 10: ", msg_level)
  is.null(msg_types) || is.character(msg_types) ||
    stop("'msg_types' must be NULL or a character vector: ", msg_types)
  all(is_in(msg_types, c("INFO", "WARNING", "ERROR"))) ||
    stop("'msg_types' must be either 'INFO', 'WARNING' or 'ERROR': ", msg_types)
  is.character(log_path) && length(log_path) == 1 ||
    stop("'log_path' must be a string: ", log_path)

  if (!is_type(x, type = type, n = n)) {
    prefix <- ""
    if (sys.nframe() > 1) {
      calling_function <- deparse(sys.calls()[[sys.nframe() - 1]][[1]])
      prefix <- paste0("In ", calling_function, "(): ")
    }

    msg <- paste0("'", deparse(substitute(x)), "' must have type '", type, "'")
    if (!is.null(n)) {
      msg <- paste(msg, "and of length", n)
    }

    error(
      prefix,
      msg,
      level     = level,
      msg_level = msg_level,
      msg_types = msg_types,
      log_path  = log_path
    )
  }

  invisible(TRUE)
}

#  FUNCTION: assert_class ------------------------------------------------------
#
#' @param class (character) The allowed class.
#'
#' @rdname assert_type
#' @export
#'
assert_class <- function(
  x,
  class,
  n         = NULL,
  level     = 1,
  msg_level = getOption("msgr.level"),
  msg_types = getOption("msgr.types"),
  log_path  = getOption("msgr.log_path")
) {
  is.character(class) && length(class) == 1 ||
    stop("'class' must be a character vector of length 1")
  is.null(n) || is_natural(n, n = 1) ||
    stop("'n' must be a positive integer")
  is_natural(level, n = 1) && is_in_range(level, min = 1, max = 10) ||
    stop("'level' must be an integer between 1 and 10: ", level)
  is_natural(msg_level, n = 1) && is_in_range(msg_level, min = 1, max = 10) ||
    stop("'msg_level' must be an integer between 1 and 10: ", msg_level)
  is.null(msg_types) || is.character(msg_types) ||
    stop("'msg_types' must be NULL or a character vector: ", msg_types)
  all(is_in(msg_types, c("INFO", "WARNING", "ERROR"))) ||
    stop("'msg_types' must be either 'INFO', 'WARNING' or 'ERROR': ", msg_types)
  is.character(log_path) && length(log_path) == 1 ||
    stop("'log_path' must be a string: ", log_path)

  if (!is_class(x, class = class, n = n)) {
    prefix <- ""
    if (sys.nframe() > 1) {
      calling_function <- deparse(sys.calls()[[sys.nframe() - 1]][[1]])
      prefix <- paste0("In ", calling_function, "(): ")
    }

    msg <- paste0(
      "'", deparse(substitute(x)), "' must have class '", class, "'"
    )
    if (!is.null(n)) {
      msg <- paste(msg, "and of length", n)
    }

    error(
      prefix,
      msg,
      level     = level,
      msg_level = msg_level,
      msg_types = msg_types,
      log_path  = log_path
    )
  }

  invisible(TRUE)
}

#  FUNCTION: assert_null -------------------------------------------------------
#
#' @rdname assert_type
#' @export
#'
assert_null <- function(
  x,
  level     = 1,
  msg_level = getOption("msgr.level"),
  msg_types = getOption("msgr.types"),
  log_path  = getOption("msgr.log_path")
) {
  is_natural(level, n = 1) && is_in_range(level, min = 1, max = 10) ||
    stop("'level' must be an integer between 1 and 10: ", level)
  is_natural(msg_level, n = 1) && is_in_range(msg_level, min = 1, max = 10) ||
    stop("'msg_level' must be an integer between 1 and 10: ", msg_level)
  is.null(msg_types) || is.character(msg_types) ||
    stop("'msg_types' must be NULL or a character vector: ", msg_types)
  all(is_in(msg_types, c("INFO", "WARNING", "ERROR"))) ||
    stop("'msg_types' must be either 'INFO', 'WARNING' or 'ERROR': ", msg_types)
  is.character(log_path) && length(log_path) == 1 ||
    stop("'log_path' must be a string: ", log_path)

  if (!is.null(x)) {
    prefix <- ""
    if (sys.nframe() > 1) {
      calling_function <- deparse(sys.calls()[[sys.nframe() - 1]][[1]])
      prefix <- paste0("In ", calling_function, "(): ")
    }

    error(
      prefix,
      paste0("'", deparse(substitute(x)), "' must be NULL"),
      level     = level,
      msg_level = msg_level,
      msg_types = msg_types,
      log_path  = log_path
    )
  }

  invisible(TRUE)
}

#  FUNCTION: assert_atomic -----------------------------------------------------
#
#' @rdname assert_type
#' @export
#'
assert_atomic <- function(
  x,
  n         = NULL,
  level     = 1,
  msg_level = getOption("msgr.level"),
  msg_types = getOption("msgr.types"),
  log_path  = getOption("msgr.log_path")
) {
  is.null(n) || is_natural(n, n = 1) ||
    stop("'n' must be a positive integer")
  is_natural(level, n = 1) && is_in_range(level, min = 1, max = 10) ||
    stop("'level' must be an integer between 1 and 10: ", level)
  is_natural(msg_level, n = 1) && is_in_range(msg_level, min = 1, max = 10) ||
    stop("'msg_level' must be an integer between 1 and 10: ", msg_level)
  is.null(msg_types) || is.character(msg_types) ||
    stop("'msg_types' must be NULL or a character vector: ", msg_types)
  all(is_in(msg_types, c("INFO", "WARNING", "ERROR"))) ||
    stop("'msg_types' must be either 'INFO', 'WARNING' or 'ERROR': ", msg_types)
  is.character(log_path) && length(log_path) == 1 ||
    stop("'log_path' must be a string: ", log_path)

  if (!rlang::is_atomic(x, n = n)) {
    prefix <- ""
    if (sys.nframe() > 1) {
      calling_function <- deparse(sys.calls()[[sys.nframe() - 1]][[1]])
      prefix <- paste0("In ", calling_function, "(): ")
    }

    msg <- paste0("'", deparse(substitute(x)), "' must be an atomic vector")
    if (!is.null(n)) {
      msg <- paste(msg, "of length", n)
    }

    error(
      prefix,
      msg,
      level     = level,
      msg_level = msg_level,
      msg_types = msg_types,
      log_path  = log_path
    )
  }

  invisible(TRUE)
}

#  FUNCTION: assert_vector -----------------------------------------------------
#
#' @rdname assert_type
#' @export
#'
assert_vector <- function(
  x,
  n         = NULL,
  level     = 1,
  msg_level = getOption("msgr.level"),
  msg_types = getOption("msgr.types"),
  log_path  = getOption("msgr.log_path")
) {
  is.null(n) || is_natural(n, n = 1) ||
    stop("'n' must be a positive integer")
  is_natural(level, n = 1) && is_in_range(level, min = 1, max = 10) ||
    stop("'level' must be an integer between 1 and 10: ", level)
  is_natural(msg_level, n = 1) && is_in_range(msg_level, min = 1, max = 10) ||
    stop("'msg_level' must be an integer between 1 and 10: ", msg_level)
  is.null(msg_types) || is.character(msg_types) ||
    stop("'msg_types' must be NULL or a character vector: ", msg_types)
  all(is_in(msg_types, c("INFO", "WARNING", "ERROR"))) ||
    stop("'msg_types' must be either 'INFO', 'WARNING' or 'ERROR': ", msg_types)
  is.character(log_path) && length(log_path) == 1 ||
    stop("'log_path' must be a string: ", log_path)

  if (!rlang::is_vector(x, n = n)) {
    prefix <- ""
    if (sys.nframe() > 1) {
      calling_function <- deparse(sys.calls()[[sys.nframe() - 1]][[1]])
      prefix <- paste0("In ", calling_function, "(): ")
    }

    msg <- paste0("'", deparse(substitute(x)), "' must be a vector")
    if (!is.null(n)) {
      msg <- paste(msg, "of length", n)
    }

    error(
      prefix,
      msg,
      level     = level,
      msg_level = msg_level,
      msg_types = msg_types,
      log_path  = log_path
    )
  }

  invisible(TRUE)
}

#  FUNCTION: assert_logical ----------------------------------------------------
#
#' @rdname assert_type
#' @export
#'
assert_logical <- function(
  x,
  n         = NULL,
  level     = 1,
  msg_level = getOption("msgr.level"),
  msg_types = getOption("msgr.types"),
  log_path  = getOption("msgr.log_path")
) {
  is.null(n) || is_natural(n, n = 1) ||
    stop("'n' must be a positive integer")
  is_natural(level, n = 1) && is_in_range(level, min = 1, max = 10) ||
    stop("'level' must be an integer between 1 and 10: ", level)
  is_natural(msg_level, n = 1) && is_in_range(msg_level, min = 1, max = 10) ||
    stop("'msg_level' must be an integer between 1 and 10: ", msg_level)
  is.null(msg_types) || is.character(msg_types) ||
    stop("'msg_types' must be NULL or a character vector: ", msg_types)
  all(is_in(msg_types, c("INFO", "WARNING", "ERROR"))) ||
    stop("'msg_types' must be either 'INFO', 'WARNING' or 'ERROR': ", msg_types)
  is.character(log_path) && length(log_path) == 1 ||
    stop("'log_path' must be a string: ", log_path)

  if (!rlang::is_logical(x, n = n)) {
    prefix <- ""
    if (sys.nframe() > 1) {
      calling_function <- deparse(sys.calls()[[sys.nframe() - 1]][[1]])
      prefix <- paste0("In ", calling_function, "(): ")
    }

    msg <- paste0("'", deparse(substitute(x)), "' must be a logical vector")
    if (!is.null(n)) {
      msg <- paste(msg, "of length", n)
    }

    error(
      prefix,
      msg,
      level     = level,
      msg_level = msg_level,
      msg_types = msg_types,
      log_path  = log_path
    )
  }

  invisible(TRUE)
}

#  FUNCTION: assert_integer ----------------------------------------------------
#
#' @rdname assert_type
#' @export
#'
assert_integer <- function(
  x,
  n         = NULL,
  level     = 1,
  msg_level = getOption("msgr.level"),
  msg_types = getOption("msgr.types"),
  log_path  = getOption("msgr.log_path")
) {
  is.null(n) || is_natural(n, n = 1) ||
    stop("'n' must be a positive integer")
  is_natural(level, n = 1) && is_in_range(level, min = 1, max = 10) ||
    stop("'level' must be an integer between 1 and 10: ", level)
  is_natural(msg_level, n = 1) && is_in_range(msg_level, min = 1, max = 10) ||
    stop("'msg_level' must be an integer between 1 and 10: ", msg_level)
  is.null(msg_types) || is.character(msg_types) ||
    stop("'msg_types' must be NULL or a character vector: ", msg_types)
  all(is_in(msg_types, c("INFO", "WARNING", "ERROR"))) ||
    stop("'msg_types' must be either 'INFO', 'WARNING' or 'ERROR': ", msg_types)
  is.character(log_path) && length(log_path) == 1 ||
    stop("'log_path' must be a string: ", log_path)

  if (!rlang::is_integer(x, n = n)) {
    prefix <- ""
    if (sys.nframe() > 1) {
      calling_function <- deparse(sys.calls()[[sys.nframe() - 1]][[1]])
      prefix <- paste0("In ", calling_function, "(): ")
    }

    msg <- paste0("'", deparse(substitute(x)), "' must be an integer vector")
    if (!is.null(n)) {
      msg <- paste(msg, "of length", n)
    }

    error(
      prefix,
      msg,
      level     = level,
      msg_level = msg_level,
      msg_types = msg_types,
      log_path  = log_path
    )
  }

  invisible(TRUE)
}

#  FUNCTION: assert_natural ----------------------------------------------------
#
#' @rdname assert_type
#' @export
#'
assert_natural <- function(
  x,
  n         = NULL,
  level     = 1,
  msg_level = getOption("msgr.level"),
  msg_types = getOption("msgr.types"),
  log_path  = getOption("msgr.log_path")
) {
  is.null(n) || is_natural(n, n = 1) ||
    stop("'n' must be a positive integer")
  is_natural(level, n = 1) && is_in_range(level, min = 1, max = 10) ||
    stop("'level' must be an integer between 1 and 10: ", level)
  is_natural(msg_level, n = 1) && is_in_range(msg_level, min = 1, max = 10) ||
    stop("'msg_level' must be an integer between 1 and 10: ", msg_level)
  is.null(msg_types) || is.character(msg_types) ||
    stop("'msg_types' must be NULL or a character vector: ", msg_types)
  all(is_in(msg_types, c("INFO", "WARNING", "ERROR"))) ||
    stop("'msg_types' must be either 'INFO', 'WARNING' or 'ERROR': ", msg_types)
  is.character(log_path) && length(log_path) == 1 ||
    stop("'log_path' must be a string: ", log_path)

  if (!is_natural(x, n = n)) {
    prefix <- ""
    if (sys.nframe() > 1) {
      calling_function <- deparse(sys.calls()[[sys.nframe() - 1]][[1]])
      prefix <- paste0("In ", calling_function, "(): ")
    }

    msg <- paste0(
      "'", deparse(substitute(x)), "' must be a positive integer vector"
    )
    if (!is.null(n)) {
      msg <- paste(msg, "of length", n)
    }

    error(
      prefix,
      msg,
      level     = level,
      msg_level = msg_level,
      msg_types = msg_types,
      log_path  = log_path
    )
  }

  invisible(TRUE)
}

#  FUNCTION: assert_double -----------------------------------------------------
#
#' @rdname assert_type
#' @export
#'
assert_double <- function(
  x,
  n         = NULL,
  level     = 1,
  msg_level = getOption("msgr.level"),
  msg_types = getOption("msgr.types"),
  log_path  = getOption("msgr.log_path")
) {
  is.null(n) || is_natural(n, n = 1) ||
    stop("'n' must be a positive integer")
  is_natural(level, n = 1) && is_in_range(level, min = 1, max = 10) ||
    stop("'level' must be an integer between 1 and 10: ", level)
  is_natural(msg_level, n = 1) && is_in_range(msg_level, min = 1, max = 10) ||
    stop("'msg_level' must be an integer between 1 and 10: ", msg_level)
  is.null(msg_types) || is.character(msg_types) ||
    stop("'msg_types' must be NULL or a character vector: ", msg_types)
  all(is_in(msg_types, c("INFO", "WARNING", "ERROR"))) ||
    stop("'msg_types' must be either 'INFO', 'WARNING' or 'ERROR': ", msg_types)
  is.character(log_path) && length(log_path) == 1 ||
    stop("'log_path' must be a string: ", log_path)

  if (!rlang::is_double(x, n = n)) {
    prefix <- ""
    if (sys.nframe() > 1) {
      calling_function <- deparse(sys.calls()[[sys.nframe() - 1]][[1]])
      prefix <- paste0("In ", calling_function, "(): ")
    }

    msg <- paste0("'", deparse(substitute(x)), "' must be a double vector")
    if (!is.null(n)) {
      msg <- paste(msg, "of length", n)
    }

    error(
      prefix,
      msg,
      level     = level,
      msg_level = msg_level,
      msg_types = msg_types,
      log_path  = log_path
    )
  }

  invisible(TRUE)
}

#  FUNCTION: assert_number -----------------------------------------------------
#
#' @rdname assert_type
#' @export
#'
assert_number <- function(
  x,
  n         = NULL,
  level     = 1,
  msg_level = getOption("msgr.level"),
  msg_types = getOption("msgr.types"),
  log_path  = getOption("msgr.log_path")
) {
  is.null(n) || is_natural(n, n = 1) ||
    stop("'n' must be a positive integer")
  is_natural(level, n = 1) && is_in_range(level, min = 1, max = 10) ||
    stop("'level' must be an integer between 1 and 10: ", level)
  is_natural(msg_level, n = 1) && is_in_range(msg_level, min = 1, max = 10) ||
    stop("'msg_level' must be an integer between 1 and 10: ", msg_level)
  is.null(msg_types) || is.character(msg_types) ||
    stop("'msg_types' must be NULL or a character vector: ", msg_types)
  all(is_in(msg_types, c("INFO", "WARNING", "ERROR"))) ||
    stop("'msg_types' must be either 'INFO', 'WARNING' or 'ERROR': ", msg_types)
  is.character(log_path) && length(log_path) == 1 ||
    stop("'log_path' must be a string: ", log_path)

  if (!is_number(x, n = n)) {
    prefix <- ""
    if (sys.nframe() > 1) {
      calling_function <- deparse(sys.calls()[[sys.nframe() - 1]][[1]])
      prefix <- paste0("In ", calling_function, "(): ")
    }

    msg <- paste0("'", deparse(substitute(x)), "' must be a numeric vector")
    if (!is.null(n)) {
      msg <- paste(msg, "of length", n)
    }

    error(
      prefix,
      msg,
      level     = level,
      msg_level = msg_level,
      msg_types = msg_types,
      log_path  = log_path
    )
  }

  invisible(TRUE)
}

#  FUNCTION: assert_character --------------------------------------------------
#
#' @rdname assert_type
#' @export
#'
assert_character <- function(
  x,
  n         = NULL,
  level     = 1,
  msg_level = getOption("msgr.level"),
  msg_types = getOption("msgr.types"),
  log_path  = getOption("msgr.log_path")
) {
  is.null(n) || is_natural(n, n = 1) ||
    stop("'n' must be a positive integer")
  is_natural(level, n = 1) && is_in_range(level, min = 1, max = 10) ||
    stop("'level' must be an integer between 1 and 10: ", level)
  is_natural(msg_level, n = 1) && is_in_range(msg_level, min = 1, max = 10) ||
    stop("'msg_level' must be an integer between 1 and 10: ", msg_level)
  is.null(msg_types) || is.character(msg_types) ||
    stop("'msg_types' must be NULL or a character vector: ", msg_types)
  all(is_in(msg_types, c("INFO", "WARNING", "ERROR"))) ||
    stop("'msg_types' must be either 'INFO', 'WARNING' or 'ERROR': ", msg_types)
  is.character(log_path) && length(log_path) == 1 ||
    stop("'log_path' must be a string: ", log_path)

  if (!rlang::is_character(x, n = n)) {
    prefix <- ""
    if (sys.nframe() > 1) {
      calling_function <- deparse(sys.calls()[[sys.nframe() - 1]][[1]])
      prefix <- paste0("In ", calling_function, "(): ")
    }

    msg <- paste0("'", deparse(substitute(x)), "' must be a character vector")
    if (!is.null(n)) {
      msg <- paste(msg, "of length", n)
    }

    error(
      prefix,
      msg,
      level     = level,
      msg_level = msg_level,
      msg_types = msg_types,
      log_path  = log_path
    )
  }

  invisible(TRUE)
}

#  FUNCTION: assert_factor -----------------------------------------------------
#
#' @param levels (character, optional) The allowed levels. If `NULL` the levels
#'   are not checked. Default: `NULL`.
#'
#' @rdname assert_type
#' @export
#'
assert_factor <- function(
  x,
  levels    = NULL,
  n         = NULL,
  level     = 1,
  msg_level = getOption("msgr.level"),
  msg_types = getOption("msgr.types"),
  log_path  = getOption("msgr.log_path")
) {
  is.null(levels) || is.character(levels) ||
    stop("'levels' must be a character vector")
  is.null(n) || is_natural(n, n = 1) ||
    stop("'n' must be a positive integer")
  is_natural(level, n = 1) && is_in_range(level, min = 1, max = 10) ||
    stop("'level' must be an integer between 1 and 10: ", level)
  is_natural(msg_level, n = 1) && is_in_range(msg_level, min = 1, max = 10) ||
    stop("'msg_level' must be an integer between 1 and 10: ", msg_level)
  is.null(msg_types) || is.character(msg_types) ||
    stop("'msg_types' must be NULL or a character vector: ", msg_types)
  all(is_in(msg_types, c("INFO", "WARNING", "ERROR"))) ||
    stop("'msg_types' must be either 'INFO', 'WARNING' or 'ERROR': ", msg_types)
  is.character(log_path) && length(log_path) == 1 ||
    stop("'log_path' must be a string: ", log_path)

  if (!is_factor(x, levels = levels, n = n)) {
    prefix <- ""
    if (sys.nframe() > 1) {
      calling_function <- deparse(sys.calls()[[sys.nframe() - 1]][[1]])
      prefix <- paste0("In ", calling_function, "(): ")
    }

    msg <- paste0("'", deparse(substitute(x)), "' must be a factor vector")
    if (!is.null(levels)) {
      msg <- paste0(
        msg, " with levels '", paste(levels, collapse = "', '"), "'"
      )
    }
    if (!is.null(n)) {
      msg <- paste(msg, "of length", n)
    }

    error(
      prefix,
      msg,
      level     = level,
      msg_level = msg_level,
      msg_types = msg_types,
      log_path  = log_path
    )
  }

  invisible(TRUE)
}

#  FUNCTION: assert_list -------------------------------------------------------
#
#' @rdname assert_type
#' @export
#'
assert_list <- function(
  x,
  n         = NULL,
  level     = 1,
  msg_level = getOption("msgr.level"),
  msg_types = getOption("msgr.types"),
  log_path  = getOption("msgr.log_path")
) {
  is.null(n) || is_natural(n, n = 1) ||
    stop("'n' must be a positive integer")
  is_natural(level, n = 1) && is_in_range(level, min = 1, max = 10) ||
    stop("'level' must be an integer between 1 and 10: ", level)
  is_natural(msg_level, n = 1) && is_in_range(msg_level, min = 1, max = 10) ||
    stop("'msg_level' must be an integer between 1 and 10: ", msg_level)
  is.null(msg_types) || is.character(msg_types) ||
    stop("'msg_types' must be NULL or a character vector: ", msg_types)
  all(is_in(msg_types, c("INFO", "WARNING", "ERROR"))) ||
    stop("'msg_types' must be either 'INFO', 'WARNING' or 'ERROR': ", msg_types)
  is.character(log_path) && length(log_path) == 1 ||
    stop("'log_path' must be a string: ", log_path)

  if (!rlang::is_list(x, n = n)) {
    prefix <- ""
    if (sys.nframe() > 1) {
      calling_function <- deparse(sys.calls()[[sys.nframe() - 1]][[1]])
      prefix <- paste0("In ", calling_function, "(): ")
    }

    msg <- paste0("'", deparse(substitute(x)), "' must be a list")
    if (!is.null(n)) {
      msg <- paste(msg, "of length", n)
    }

    error(
      prefix,
      msg,
      level     = level,
      msg_level = msg_level,
      msg_types = msg_types,
      log_path  = log_path
    )
  }

  invisible(TRUE)
}

#  FUNCTION: assert_array ------------------------------------------------------
#
#' @param dims (integer, optional) The allowed dimension sizes. If `NULL` the
#'   dimensions are not checked. Default: `NULL`.
#'
#' @rdname assert_type
#' @export
#'
assert_array <- function(
  x,
  dims      = NULL,
  level     = 1,
  msg_level = getOption("msgr.level"),
  msg_types = getOption("msgr.types"),
  log_path  = getOption("msgr.log_path")
) {
  is.null(dims) || is_natural(dims) ||
    stop("'dims' must be a positive integer vector")
  is_natural(level, n = 1) && is_in_range(level, min = 1, max = 10) ||
    stop("'level' must be an integer between 1 and 10: ", level)
  is_natural(msg_level, n = 1) && is_in_range(msg_level, min = 1, max = 10) ||
    stop("'msg_level' must be an integer between 1 and 10: ", msg_level)
  is.null(msg_types) || is.character(msg_types) ||
    stop("'msg_types' must be NULL or a character vector: ", msg_types)
  all(is_in(msg_types, c("INFO", "WARNING", "ERROR"))) ||
    stop("'msg_types' must be either 'INFO', 'WARNING' or 'ERROR': ", msg_types)
  is.character(log_path) && length(log_path) == 1 ||
    stop("'log_path' must be a string: ", log_path)

  if (!is_array(x, dims = dims)) {
    prefix <- ""
    if (sys.nframe() > 1) {
      calling_function <- deparse(sys.calls()[[sys.nframe() - 1]][[1]])
      prefix <- paste0("In ", calling_function, "(): ")
    }

    msg <- paste0("'", deparse(substitute(x)), "' must be an array")
    if (!is.null(dims)) {
      msg <- paste(msg, "with dimensions", paste(dims, collapse = " x "))
    }

    error(
      prefix,
      msg,
      level     = level,
      msg_level = msg_level,
      msg_types = msg_types,
      log_path  = log_path
    )
  }

  invisible(TRUE)
}

#  FUNCTION: assert_matrix -----------------------------------------------------
#
#' @param n_col (integer, optional) The allowed number of columns. If `NULL` the
#'   columns are not checked. Default: `NULL`.
#' @param n_row (integer, optional) The allowed number of rows. If `NULL` the
#'   rows are not checked. Default: `NULL`.
#'
#' @rdname assert_type
#' @export
#'
assert_matrix <- function(
  x,
  n_col     = NULL,
  n_row     = NULL,
  level     = 1,
  msg_level = getOption("msgr.level"),
  msg_types = getOption("msgr.types"),
  log_path  = getOption("msgr.log_path")
) {
  is.null(n_col) || is_natural(n_col, n = 1) ||
    stop("'n_col' must be a positive integer")
  is.null(n_row) || is_natural(n_row, n = 1) ||
    stop("'n_row' must be a positive integer")
  is_natural(level, n = 1) && is_in_range(level, min = 1, max = 10) ||
    stop("'level' must be an integer between 1 and 10: ", level)
  is_natural(msg_level, n = 1) && is_in_range(msg_level, min = 1, max = 10) ||
    stop("'msg_level' must be an integer between 1 and 10: ", msg_level)
  is.null(msg_types) || is.character(msg_types) ||
    stop("'msg_types' must be NULL or a character vector: ", msg_types)
  all(is_in(msg_types, c("INFO", "WARNING", "ERROR"))) ||
    stop("'msg_types' must be either 'INFO', 'WARNING' or 'ERROR': ", msg_types)
  is.character(log_path) && length(log_path) == 1 ||
    stop("'log_path' must be a string: ", log_path)

  if (!is_matrix(x, n_col = n_col, n_row = n_row)) {
    prefix <- ""
    if (sys.nframe() > 1) {
      calling_function <- deparse(sys.calls()[[sys.nframe() - 1]][[1]])
      prefix <- paste0("In ", calling_function, "(): ")
    }

    msg <- paste0("'", deparse(substitute(x)), "' must be a matrix")
    if (!is.null(n_col)) {
      msg <- paste(msg, "with", n_col, "columns")
    }
    if (!is.null(n_row)) {
      if (is.null(n_col)) {
        msg <- paste(msg, "with", n_row, "rows")
      } else {
        msg <- paste(msg, "and", n_row, "rows")
      }
    }

    error(
      prefix,
      msg,
      level     = level,
      msg_level = msg_level,
      msg_types = msg_types,
      log_path  = log_path
    )
  }

  invisible(TRUE)
}

#  FUNCTION: assert_data_frame -------------------------------------------------
#
#' @rdname assert_type
#' @export
#'
assert_data_frame <- function(
  x,
  n_col     = NULL,
  n_row     = NULL,
  level     = 1,
  msg_level = getOption("msgr.level"),
  msg_types = getOption("msgr.types"),
  log_path  = getOption("msgr.log_path")
) {
  is.null(n_col) || is_natural(n_col, n = 1) ||
    stop("'n_col' must be a positive integer")
  is.null(n_row) || is_natural(n_row, n = 1) ||
    stop("'n_row' must be a positive integer")
  is_natural(level, n = 1) && is_in_range(level, min = 1, max = 10) ||
    stop("'level' must be an integer between 1 and 10: ", level)
  is_natural(msg_level, n = 1) && is_in_range(msg_level, min = 1, max = 10) ||
    stop("'msg_level' must be an integer between 1 and 10: ", msg_level)
  is.null(msg_types) || is.character(msg_types) ||
    stop("'msg_types' must be NULL or a character vector: ", msg_types)
  all(is_in(msg_types, c("INFO", "WARNING", "ERROR"))) ||
    stop("'msg_types' must be either 'INFO', 'WARNING' or 'ERROR': ", msg_types)
  is.character(log_path) && length(log_path) == 1 ||
    stop("'log_path' must be a string: ", log_path)

  if (!is_data_frame(x, n_col = n_col, n_row = n_row)) {
    prefix <- ""
    if (sys.nframe() > 1) {
      calling_function <- deparse(sys.calls()[[sys.nframe() - 1]][[1]])
      prefix <- paste0("In ", calling_function, "(): ")
    }

    msg <- paste0("'", deparse(substitute(x)), "' must be a data.frame")
    if (!is.null(n_col)) {
      msg <- paste(msg, "with", n_col, "columns")
    }
    if (!is.null(n_row)) {
      if (is.null(n_col)) {
        msg <- paste(msg, "with", n_row, "rows")
      } else {
        msg <- paste(msg, "and", n_row, "rows")
      }
    }

    error(
      prefix,
      msg,
      level     = level,
      msg_level = msg_level,
      msg_types = msg_types,
      log_path  = log_path
    )
  }

  invisible(TRUE)
}

#  FUNCTION: assert_function ---------------------------------------------------
#
#' @rdname assert_type
#' @export
#'
assert_function <- function(
  x,
  level     = 1,
  msg_level = getOption("msgr.level"),
  msg_types = getOption("msgr.types"),
  log_path  = getOption("msgr.log_path")
) {
  is_natural(level, n = 1) && is_in_range(level, min = 1, max = 10) ||
    stop("'level' must be an integer between 1 and 10: ", level)
  is_natural(msg_level, n = 1) && is_in_range(msg_level, min = 1, max = 10) ||
    stop("'msg_level' must be an integer between 1 and 10: ", msg_level)
  is.null(msg_types) || is.character(msg_types) ||
    stop("'msg_types' must be NULL or a character vector: ", msg_types)
  all(is_in(msg_types, c("INFO", "WARNING", "ERROR"))) ||
    stop("'msg_types' must be either 'INFO', 'WARNING' or 'ERROR': ", msg_types)
  is.character(log_path) && length(log_path) == 1 ||
    stop("'log_path' must be a string: ", log_path)

  if (!rlang::is_function(x)) {
    prefix <- ""
    if (sys.nframe() > 1) {
      calling_function <- deparse(sys.calls()[[sys.nframe() - 1]][[1]])
      prefix <- paste0("In ", calling_function, "(): ")
    }

    error(
      prefix,
      paste0("'", deparse(substitute(x)), "' must be a function"),
      level     = level,
      msg_level = msg_level,
      msg_types = msg_types,
      log_path  = log_path
    )
  }

  invisible(TRUE)
}

#  FUNCTION: assert_formula ----------------------------------------------------
#
#' @rdname assert_type
#' @export
#'
assert_formula <- function(
  x,
  level     = 1,
  msg_level = getOption("msgr.level"),
  msg_types = getOption("msgr.types"),
  log_path  = getOption("msgr.log_path")
) {
  is_natural(level, n = 1) && is_in_range(level, min = 1, max = 10) ||
    stop("'level' must be an integer between 1 and 10: ", level)
  is_natural(msg_level, n = 1) && is_in_range(msg_level, min = 1, max = 10) ||
    stop("'msg_level' must be an integer between 1 and 10: ", msg_level)
  is.null(msg_types) || is.character(msg_types) ||
    stop("'msg_types' must be NULL or a character vector: ", msg_types)
  all(is_in(msg_types, c("INFO", "WARNING", "ERROR"))) ||
    stop("'msg_types' must be either 'INFO', 'WARNING' or 'ERROR': ", msg_types)
  is.character(log_path) && length(log_path) == 1 ||
    stop("'log_path' must be a string: ", log_path)

  if (!rlang::is_formula(x)) {
    prefix <- ""
    if (sys.nframe() > 1) {
      calling_function <- deparse(sys.calls()[[sys.nframe() - 1]][[1]])
      prefix <- paste0("In ", calling_function, "(): ")
    }

    error(
      prefix,
      paste0("'", deparse(substitute(x)), "' must be a formula"),
      level     = level,
      msg_level = msg_level,
      msg_types = msg_types,
      log_path  = log_path
    )
  }

  invisible(TRUE)
}

#  FUNCTION: assert_empty ------------------------------------------------------
#
#' Display an error if not the correct structure
#'
#' This function calls the [error()] function to display an error if the
#' structure of the input is not correct.
#'
#' The following structures can be checked:
#'
#' - `assert_empty()`: 'x' must be empty or `NULL`.
#' - `assert_names()`: 'x' must have names and optionally have the specified
#'   `names`.
#' - `assert_length()`: 'x' must have valid length. You can specify the exact
#'   length using `n` or the minimum and/or maximum length using `n_min` and
#'   `n_max` respectively.
#'
#' @param x (any) The object to test.
#' @param level (integer, optional) The level of the message, from 1 to 10.
#'   Default: 1.
#' @param msg_level (integer, optional) The maximum level of messages to output.
#'   Default: set in the option `"msgr.level"`.
#' @param msg_types (character, optional) The type to write or display. Must
#'   either NULL or one or more from "INFO", "WARNING" or "ERROR". Default: set
#'   in the option `"msgr.types"`.
#' @param log_path (character, optional) The file path to the text log file. If
#'   set to "", then no logs are written. Default: set in the option
#'   `"msgr.log_path"`.
#'
#' @return If assertion passes then `TRUE` is returned. This allows you to make
#'   multiple assertions separated by `&`.
#'
#' @examples \dontrun{
#'
#' # No error
#' assert_empty(NULL)
#' assert_empty(integer())
#' # Error
#' assert_empty(1)
#'
#' # No error
#' assert_names(c(name = "Bob", age = 42))
#' assert_names(c(name = "Bob", age = 42), names = c("name", "age"))
#' # Error
#' assert_names(c(name = "Bob", age = 42), names = c("name", "email"))
#'
#' # No error
#' assert_length(1:3, n = 3)
#' assert_length(1:3, n_min = 1)
#' assert_length(1:3, n_min = 1, n_max = 10)
#' # Error
#' assert_length(1:3, n = 1)
#' assert_length(1:3, n_min = 5)
#' assert_length(1:3, n_min = 1, n_max = 2)
#'
#' }
#'
#' @rdname assert_structure
#' @export
#'
assert_empty <- function(
  x,
  level     = 1,
  msg_level = getOption("msgr.level"),
  msg_types = getOption("msgr.types"),
  log_path  = getOption("msgr.log_path")
) {
  is_natural(level, n = 1) && is_in_range(level, min = 1, max = 10) ||
    stop("'level' must be an integer between 1 and 10: ", level)
  is_natural(msg_level, n = 1) && is_in_range(msg_level, min = 1, max = 10) ||
    stop("'msg_level' must be an integer between 1 and 10: ", msg_level)
  is.null(msg_types) || is.character(msg_types) ||
    stop("'msg_types' must be NULL or a character vector: ", msg_types)
  all(is_in(msg_types, c("INFO", "WARNING", "ERROR"))) ||
    stop("'msg_types' must be either 'INFO', 'WARNING' or 'ERROR': ", msg_types)
  is.character(log_path) && length(log_path) == 1 ||
    stop("'log_path' must be a string: ", log_path)

  if (!rlang::is_empty(x)) {
    prefix <- ""
    if (sys.nframe() > 1) {
      calling_function <- deparse(sys.calls()[[sys.nframe() - 1]][[1]])
      prefix <- paste0("In ", calling_function, "(): ")
    }

    error(
      prefix,
      paste0("'", deparse(substitute(x)), "' must be empty or 'NULL'"),
      level     = level,
      msg_level = msg_level,
      msg_types = msg_types,
      log_path  = log_path
    )
  }

  invisible(TRUE)
}

#  FUNCTION: assert_names ------------------------------------------------------
#
#' @param names (character) The allowed names.
#'
#' @rdname assert_structure
#' @export
#'
assert_names <- function(
  x,
  names     = NULL,
  level     = 1,
  msg_level = getOption("msgr.level"),
  msg_types = getOption("msgr.types"),
  log_path  = getOption("msgr.log_path")
) {
  is.null(names) || is.character(names) ||
    stop("'names' must be a character vector")
  is_natural(level, n = 1) && is_in_range(level, min = 1, max = 10) ||
    stop("'level' must be an integer between 1 and 10: ", level)
  is_natural(msg_level, n = 1) && is_in_range(msg_level, min = 1, max = 10) ||
    stop("'msg_level' must be an integer between 1 and 10: ", msg_level)
  is.null(msg_types) || is.character(msg_types) ||
    stop("'msg_types' must be NULL or a character vector: ", msg_types)
  all(is_in(msg_types, c("INFO", "WARNING", "ERROR"))) ||
    stop("'msg_types' must be either 'INFO', 'WARNING' or 'ERROR': ", msg_types)
  is.character(log_path) && length(log_path) == 1 ||
    stop("'log_path' must be a string: ", log_path)

  if (!all(has_names(x, names = names))) {
    prefix <- ""
    if (sys.nframe() > 1) {
      calling_function <- deparse(sys.calls()[[sys.nframe() - 1]][[1]])
      prefix <- paste0("In ", calling_function, "(): ")
    }

    msg <- paste0("'", deparse(substitute(x)), "' must have names")
    if (!is.null(names)) {
      msg <- paste0(msg, " in '", paste(names, collapse = "', '"), "'")
    }

    error(
      prefix,
      msg,
      level     = level,
      msg_level = msg_level,
      msg_types = msg_types,
      log_path  = log_path
    )
  }

  invisible(TRUE)
}

#  FUNCTION: assert_length -----------------------------------------------------
#
#' @param n (integer, optional) The allowed length.
#' @param n_min (integer, optional) The minimum allowed length.
#' @param n_max (integer, optional) The maximum allowed length.
#'
#' @rdname assert_structure
#' @export
#'
assert_length <- function(
  x,
  n         = NULL,
  n_min     = NULL,
  n_max     = NULL,
  level     = 1,
  msg_level = getOption("msgr.level"),
  msg_types = getOption("msgr.types"),
  log_path  = getOption("msgr.log_path")
) {
  is.null(n) || is_natural(n, n = 1) ||
    stop("'n' must be a positive integer")
  is.null(n_min) || is_natural(n_min, n = 1) ||
    stop("'n_min' must be a positive integer")
  is.null(n_max) || is_natural(n_max, n = 1) ||
    stop("'n_max' must be a positive integer")
  is_natural(level, n = 1) && is_in_range(level, min = 1, max = 10) ||
    stop("'level' must be an integer between 1 and 10: ", level)
  is_natural(msg_level, n = 1) && is_in_range(msg_level, min = 1, max = 10) ||
    stop("'msg_level' must be an integer between 1 and 10: ", msg_level)
  is.null(msg_types) || is.character(msg_types) ||
    stop("'msg_types' must be NULL or a character vector: ", msg_types)
  all(is_in(msg_types, c("INFO", "WARNING", "ERROR"))) ||
    stop("'msg_types' must be either 'INFO', 'WARNING' or 'ERROR': ", msg_types)
  is.character(log_path) && length(log_path) == 1 ||
    stop("'log_path' must be a string: ", log_path)

  if (!has_length(x, n = n, n_min = n_min, n_max = n_max)) {
    prefix <- ""
    if (sys.nframe() > 1) {
      calling_function <- deparse(sys.calls()[[sys.nframe() - 1]][[1]])
      prefix <- paste0("In ", calling_function, "(): ")
    }

    msg <- character()
    if (!is.null(n)) {
      msg <- c(msg, paste("==", n))
    }
    if (!is.null(n_min)) {
      msg <- c(msg, paste(">=", n_min))
    }
    if (!is.null(n_max)) {
      msg <- c(msg, paste("<=", n_max))
    }

    msg <- paste0(
      "'", deparse(substitute(x)), "' must have length ",
      paste(msg, collapse = " and ")
    )

    error(
      prefix,
      msg,
      level     = level,
      msg_level = msg_level,
      msg_types = msg_types,
      log_path  = log_path
    )
  }

  invisible(TRUE)
}

#  FUNCTION: assert_na ---------------------------------------------------------
#
#' Display an error if not the allowed values
#'
#' This function calls the [error()] function to display an error if the
#' variable's values are not valid.
#'
#' The following validations can be checked:
#'
#' The following values can be checked:
#'
#' - `assert_na()`: All elements of 'x' must be `NA`.
#' - `assert_in()`: All elements of 'x' must in specified `values`.
#' - `assert_in_range()`: All elements of 'x' must be in the specified numeric
#'   range.
#' - `assert_char_length`: All elements of 'x' must have valid character length.
#'   You can specify the exact length using `n` or the minimum and/or maximum
#'   length using `n_min` and `n_max` respectively.
#'
#' @param x (any) The variable to check.
#' @param level (integer, optional) The level of the message, from 1 to 10.
#'   Default: 1.
#' @param msg_level (integer, optional) The maximum level of messages to output.
#'   Default: set in the option `"msgr.level"`.
#' @param msg_types (character, optional) The type to write or display. Must
#'   either NULL or one or more from "INFO", "WARNING" or "ERROR". Default: set
#'   in the option `"msgr.types"`.
#' @param log_path (character, optional) The file path to the text log file. If
#'   set to "", then no logs are written. Default: set in the option
#'   `"msgr.log_path"`.
#' @return If assertion passes then `TRUE` is returned. This allows you to make
#'   multiple assertions separated by `&`.
#'
#' @examples \dontrun{
#'
#' # No error
#' assert_na(NA)
#' assert_na(c(NA, NA, NA))
#' # Error
#' assert_na(1)
#' assert_na(c(1, NA, 3))
#'
#' # No error
#' assert_in(1:3, 1:10)
#' assert_in(c("A", "B"), LETTERS)
#' # Error
#' assert_in(8:12, 1:10)
#' assert_in(c("A", "B"), letters)
#'
#' # No error
#' assert_in_range(1:3, min = 1)
#' assert_in_range(1:3, min = 1, max = 10)
#' # Error
#' assert_in_range(1:3, min = 5)
#' assert_in_range(1:3, min = 1, max = 2)
#'
#' # Error
#' assert_char_length(c("bob", "jan"), n = 3)
#' assert_char_length(c("bob", "jan"), n_min = 1)
#' assert_char_length(c("bob", "jan"), n_min = 1, n_max = 10)
#' # No error
#' assert_char_length(c("bob", "jane"), n = 3)
#' assert_char_length(c("bob", "jane"), n_min = 5)
#' assert_char_length(c("bob", "jane"), n_min = 1, n_max = 2)
#'
#' }
#'
#' @rdname assert_values
#' @export
#'
assert_na <- function(
  x,
  level     = 1,
  msg_level = getOption("msgr.level"),
  msg_types = getOption("msgr.types"),
  log_path  = getOption("msgr.log_path")
) {
  is_natural(level, n = 1) && is_in_range(level, min = 1, max = 10) ||
    stop("'level' must be an integer between 1 and 10: ", level)
  is_natural(msg_level, n = 1) && is_in_range(msg_level, min = 1, max = 10) ||
    stop("'msg_level' must be an integer between 1 and 10: ", msg_level)
  is.null(msg_types) || is.character(msg_types) ||
    stop("'msg_types' must be NULL or a character vector: ", msg_types)
  all(is_in(msg_types, c("INFO", "WARNING", "ERROR"))) ||
    stop("'msg_types' must be either 'INFO', 'WARNING' or 'ERROR': ", msg_types)
  is.character(log_path) && length(log_path) == 1 ||
    stop("'log_path' must be a string: ", log_path)

  if (!all(is_na(x))) {
    prefix <- ""
    if (sys.nframe() > 1) {
      calling_function <- deparse(sys.calls()[[sys.nframe() - 1]][[1]])
      prefix <- paste0("In ", calling_function, "(): ")
    }

    error(
      prefix,
      paste0("'", deparse(substitute(x)), "' must be a vector of NAs"),
      level     = level,
      msg_level = msg_level,
      msg_types = msg_types,
      log_path  = log_path
    )
  }

  invisible(TRUE)
}

#  FUNCTION: assert_in ---------------------------------------------------------
#
#' @param values (character) The allowed values.
#'
#' @rdname assert_values
#' @export
#'
assert_in <- function(
  x,
  values,
  level     = 1,
  msg_level = getOption("msgr.level"),
  msg_types = getOption("msgr.types"),
  log_path  = getOption("msgr.log_path")
) {
  rlang::is_vector(values) ||
    stop("'values' must be a vector")
  is_natural(level, n = 1) && is_in_range(level, min = 1, max = 10) ||
    stop("'level' must be an integer between 1 and 10: ", level)
  is_natural(msg_level, n = 1) && is_in_range(msg_level, min = 1, max = 10) ||
    stop("'msg_level' must be an integer between 1 and 10: ", msg_level)
  is.null(msg_types) || is.character(msg_types) ||
    stop("'msg_types' must be NULL or a character vector: ", msg_types)
  all(is_in(msg_types, c("INFO", "WARNING", "ERROR"))) ||
    stop("'msg_types' must be either 'INFO', 'WARNING' or 'ERROR': ", msg_types)
  is.character(log_path) && length(log_path) == 1 ||
    stop("'log_path' must be a string: ", log_path)

  if (!all(is_in(x, values = values))) {
    prefix <- ""
    if (sys.nframe() > 1) {
      calling_function <- deparse(sys.calls()[[sys.nframe() - 1]][[1]])
      prefix <- paste0("In ", calling_function, "(): ")
    }

    msg <- paste0(
      "'", deparse(substitute(x)), "' must be in '",
      paste(values, collapse = "', '"), "'"
    )

    error(
      prefix,
      msg,
      level     = level,
      msg_level = msg_level,
      msg_types = msg_types,
      log_path  = log_path
    )
  }

  invisible(TRUE)
}

#  FUNCTION: assert_in_range ---------------------------------------------------
#
#' @param min (numeric, optional) The minimum allowed value for a range. If
#'   `NULL` the minimum value is not checked. Default: `NULL`.
#' @param max (numeric, optional) The maximum allowed value for a range. If
#'   `NULL` the maximum value is not checked. Default: `NULL`.
#'
#' @rdname assert_values
#' @export
#'
assert_in_range <- function(
  x,
  min       = NULL,
  max       = NULL,
  level     = 1,
  msg_level = getOption("msgr.level"),
  msg_types = getOption("msgr.types"),
  log_path  = getOption("msgr.log_path")
) {
  is.null(min) || (is_number(min, n = 1) && min >= 0) ||
    stop("'min' must be a positive number")
  is.null(max) || (is_number(max, n = 1) && max >= 0) ||
    stop("'max' must be a positive number")
  is_natural(level, n = 1) && is_in_range(level, min = 1, max = 10) ||
    stop("'level' must be an integer between 1 and 10: ", level)
  is_natural(msg_level, n = 1) && is_in_range(msg_level, min = 1, max = 10) ||
    stop("'msg_level' must be an integer between 1 and 10: ", msg_level)
  is.null(msg_types) || is.character(msg_types) ||
    stop("'msg_types' must be NULL or a character vector: ", msg_types)
  all(is_in(msg_types, c("INFO", "WARNING", "ERROR"))) ||
    stop("'msg_types' must be either 'INFO', 'WARNING' or 'ERROR': ", msg_types)
  is.character(log_path) && length(log_path) == 1 ||
    stop("'log_path' must be a string: ", log_path)

  if (!all(is_in_range(x, min = min, max = max))) {
    prefix <- ""
    if (sys.nframe() > 1) {
      calling_function <- deparse(sys.calls()[[sys.nframe() - 1]][[1]])
      prefix <- paste0("In ", calling_function, "(): ")
    }

    msg <- character()
    if (!is.null(min)) {
      msg <- c(msg, paste(">=", min))
    }
    if (!is.null(max)) {
      msg <- c(msg, paste("<=", max))
    }

    msg <- paste0(
      "'", deparse(substitute(x)), "' must have values ",
      paste(msg, collapse = " and ")
    )

    error(
      prefix,
      msg,
      level     = level,
      msg_level = msg_level,
      msg_types = msg_types,
      log_path  = log_path
    )
  }

  invisible(TRUE)
}

#  FUNCTION: assert_char_length ------------------------------------------------
#
#' @param n (integer, optional) The allowed length. If `NULL` the exact
#'   character length is not checked. Default: `NULL`.
#' @param n_min (integer, optional) The allowed minimum length. If `NULL` the
#'   minimum character length is not checked. Default: `NULL`.
#' @param n_max (integer, optional) The allowed maximum length. If `NULL` the
#'   maximum character length is not checked. Default: `NULL`.
#'
#' @rdname assert_values
#' @export
#'
assert_char_length <- function(
  x,
  n         = NULL,
  n_min     = NULL,
  n_max     = NULL,
  level     = 1,
  msg_level = getOption("msgr.level"),
  msg_types = getOption("msgr.types"),
  log_path  = getOption("msgr.log_path")
) {
  is.null(n) || is_natural(n, n = 1) ||
    stop("'n' must be a positive integer")
  is.null(n_min) || is_natural(n_min, n = 1) ||
    stop("'n_min' must be a positive integer")
  is.null(n_max) || is_natural(n_max, n = 1) ||
    stop("'n_max' must be a positive integer")
  is_natural(level, n = 1) && is_in_range(level, min = 1, max = 10) ||
    stop("'level' must be an integer between 1 and 10: ", level)
  is_natural(msg_level, n = 1) && is_in_range(msg_level, min = 1, max = 10) ||
    stop("'msg_level' must be an integer between 1 and 10: ", msg_level)
  is.null(msg_types) || is.character(msg_types) ||
    stop("'msg_types' must be NULL or a character vector: ", msg_types)
  all(is_in(msg_types, c("INFO", "WARNING", "ERROR"))) ||
    stop("'msg_types' must be either 'INFO', 'WARNING' or 'ERROR': ", msg_types)
  is.character(log_path) && length(log_path) == 1 ||
    stop("'log_path' must be a string: ", log_path)

  if (!all(has_char_length(x, n = n, n_min = n_min, n_max = n_max))) {
    prefix <- ""
    if (sys.nframe() > 1) {
      calling_function <- deparse(sys.calls()[[sys.nframe() - 1]][[1]])
      prefix <- paste0("In ", calling_function, "(): ")
    }

    msg <- character()
    if (!is.null(n)) {
      msg <- c(msg, paste("==", n))
    }
    if (!is.null(n_min)) {
      msg <- c(msg, paste(">=", n_min))
    }
    if (!is.null(n_max)) {
      msg <- c(msg, paste("<=", n_max))
    }

    msg <- paste0(
      "'", deparse(substitute(x)), "' must have character length ",
      paste(msg, collapse = " and ")
    )

    error(
      prefix,
      msg,
      level     = level,
      msg_level = msg_level,
      msg_types = msg_types,
      log_path  = log_path
    )
  }

  invisible(TRUE)
}

#  FUNCTION: assert_file -------------------------------------------------------
#
#' Display an error if a valid file or directory path
#'
#' This function calls the [error()] function to display an error if the
#' variable's values are not valid file or directory paths.
#'
#' The following validations can be checked:
#'
#' - `assert_file()`: 'x' must be an existing file
#' - `assert_dir()`: 'x' must be an existing directory
#' - `assert_readable()`: 'x' must be a readable directory or file
#' - `assert_writeable()`: 'x' must be a writeable directory or file
#' - `assert_url()`: 'x' must be a valid URL
#'
#' @param x (any) The variable to check.
#' @param level (integer, optional) The level of the message, from 1 to 10.
#'   Default: 1.
#' @param msg_level (integer, optional) The maximum level of messages to output.
#'   Default: set in the option `"msgr.level"`.
#' @param msg_types (character, optional) The type to write or display. Must
#'   either NULL or one or more from "INFO", "WARNING" or "ERROR". Default: set
#'   in the option `"msgr.types"`.
#' @param log_path (character, optional) The file path to the text log file. If
#'   set to "", then no logs are written. Default: set in the option
#'   `"msgr.log_path"`.
#'
#' @return If assertion passes then `TRUE` is returned. This allows you to make
#'   multiple assertions separated by `&`.
#'
#' @examples \dontrun{
#'
#' # No error
#' assert_file(system.file("DESCRIPTION", package = "msgr"))
#' # Error
#' assert_file("no-such-file.txt")
#'
#' # No error
#' assert_dir(R.home())
#' # Error
#' assert_dir("no-such-directory")
#'
#' # No error
#' assert_readable(R.home())
#' # Error
#' assert_readable("no-such-directory")
#'
#' # No error
#' assert_writeable(Sys.getenv("HOME"))
#' # Error
#' assert_writeable("no-such-directory")
#'
#' # No error
#' assert_url("https://www.google.com")
#' # Error
#' assert_url("no-such-site")
#'
#' }
#'
#' @export
#'
assert_file <- function(
  x,
  level     = 1,
  msg_level = getOption("msgr.level"),
  msg_types = getOption("msgr.types"),
  log_path  = getOption("msgr.log_path")
) {
  is_natural(level, n = 1) && is_in_range(level, min = 1, max = 10) ||
    stop("'level' must be an integer between 1 and 10: ", level)
  is_natural(msg_level, n = 1) && is_in_range(msg_level, min = 1, max = 10) ||
    stop("'msg_level' must be an integer between 1 and 10: ", msg_level)
  is.null(msg_types) || is.character(msg_types) ||
    stop("'msg_types' must be NULL or a character vector: ", msg_types)
  all(is_in(msg_types, c("INFO", "WARNING", "ERROR"))) ||
    stop("'msg_types' must be either 'INFO', 'WARNING' or 'ERROR': ", msg_types)
  is.character(log_path) && length(log_path) == 1 ||
    stop("'log_path' must be a string: ", log_path)

  if (!all(is_file(x))) {
    prefix <- ""
    if (sys.nframe() > 1) {
      calling_function <- deparse(sys.calls()[[sys.nframe() - 1]][[1]])
      prefix <- paste0("In ", calling_function, "(): ")
    }

    error(
      prefix,
      paste0("'", deparse(substitute(x)), "' must be an existing file"),
      level     = level,
      msg_level = msg_level,
      msg_types = msg_types,
      log_path  = log_path
    )
  }

  invisible(TRUE)
}

#  FUNCTION: assert_dir --------------------------------------------------------
#
#' @rdname assert_file
#' @export
#'
assert_dir <- function(
  x,
  level     = 1,
  msg_level = getOption("msgr.level"),
  msg_types = getOption("msgr.types"),
  log_path  = getOption("msgr.log_path")
) {
  is_natural(level, n = 1) && is_in_range(level, min = 1, max = 10) ||
    stop("'level' must be an integer between 1 and 10: ", level)
  is_natural(msg_level, n = 1) && is_in_range(msg_level, min = 1, max = 10) ||
    stop("'msg_level' must be an integer between 1 and 10: ", msg_level)
  is.null(msg_types) || is.character(msg_types) ||
    stop("'msg_types' must be NULL or a character vector: ", msg_types)
  all(is_in(msg_types, c("INFO", "WARNING", "ERROR"))) ||
    stop("'msg_types' must be either 'INFO', 'WARNING' or 'ERROR': ", msg_types)
  is.character(log_path) && length(log_path) == 1 ||
    stop("'log_path' must be a string: ", log_path)

  if (!all(is_dir(x))) {
    prefix <- ""
    if (sys.nframe() > 1) {
      calling_function <- deparse(sys.calls()[[sys.nframe() - 1]][[1]])
      prefix <- paste0("In ", calling_function, "(): ")
    }

    error(
      prefix,
      paste0("'", deparse(substitute(x)), "' must be an existing directory"),
      level     = level,
      msg_level = msg_level,
      msg_types = msg_types,
      log_path  = log_path
    )
  }

  invisible(TRUE)
}

#  FUNCTION: assert_readable ---------------------------------------------------
#
#' @rdname assert_file
#' @export
#'
assert_readable <- function(
  x,
  level     = 1,
  msg_level = getOption("msgr.level"),
  msg_types = getOption("msgr.types"),
  log_path  = getOption("msgr.log_path")
) {
  is_natural(level, n = 1) && is_in_range(level, min = 1, max = 10) ||
    stop("'level' must be an integer between 1 and 10: ", level)
  is_natural(msg_level, n = 1) && is_in_range(msg_level, min = 1, max = 10) ||
    stop("'msg_level' must be an integer between 1 and 10: ", msg_level)
  is.null(msg_types) || is.character(msg_types) ||
    stop("'msg_types' must be NULL or a character vector: ", msg_types)
  all(is_in(msg_types, c("INFO", "WARNING", "ERROR"))) ||
    stop("'msg_types' must be either 'INFO', 'WARNING' or 'ERROR': ", msg_types)
  is.character(log_path) && length(log_path) == 1 ||
    stop("'log_path' must be a string: ", log_path)

  if (!all(is_readable(x))) {
    prefix <- ""
    if (sys.nframe() > 1) {
      calling_function <- deparse(sys.calls()[[sys.nframe() - 1]][[1]])
      prefix <- paste0("In ", calling_function, "(): ")
    }

    msg <- paste0(
      "'", deparse(substitute(x)), "' must be a readable directory or file"
    )

    error(
      prefix,
      msg,
      level     = level,
      msg_level = msg_level,
      msg_types = msg_types,
      log_path  = log_path
    )
  }

  invisible(TRUE)
}

#  FUNCTION: assert_writeable --------------------------------------------------
#
#' @rdname assert_file
#' @export
#'
assert_writeable <- function(
  x,
  level     = 1,
  msg_level = getOption("msgr.level"),
  msg_types = getOption("msgr.types"),
  log_path  = getOption("msgr.log_path")
) {
  is_natural(level, n = 1) && is_in_range(level, min = 1, max = 10) ||
    stop("'level' must be an integer between 1 and 10: ", level)
  is_natural(msg_level, n = 1) && is_in_range(msg_level, min = 1, max = 10) ||
    stop("'msg_level' must be an integer between 1 and 10: ", msg_level)
  is.null(msg_types) || is.character(msg_types) ||
    stop("'msg_types' must be NULL or a character vector: ", msg_types)
  all(is_in(msg_types, c("INFO", "WARNING", "ERROR"))) ||
    stop("'msg_types' must be either 'INFO', 'WARNING' or 'ERROR': ", msg_types)
  is.character(log_path) && length(log_path) == 1 ||
    stop("'log_path' must be a string: ", log_path)

  if (!all(is_writeable(x))) {
    prefix <- ""
    if (sys.nframe() > 1) {
      calling_function <- deparse(sys.calls()[[sys.nframe() - 1]][[1]])
      prefix <- paste0("In ", calling_function, "(): ")
    }

    msg <- paste0(
      "'", deparse(substitute(x)), "' must be a writeable directory or file"
    )

    error(
      prefix,
      msg,
      level     = level,
      msg_level = msg_level,
      msg_types = msg_types,
      log_path  = log_path
    )
  }

  invisible(TRUE)
}

#  FUNCTION: assert_url --------------------------------------------------------
#
#' @rdname assert_file
#' @export
#'
assert_url <- function(
  x,
  level     = 1,
  msg_level = getOption("msgr.level"),
  msg_types = getOption("msgr.types"),
  log_path  = getOption("msgr.log_path")
) {
  is_natural(level, n = 1) && is_in_range(level, min = 1, max = 10) ||
    stop("'level' must be an integer between 1 and 10: ", level)
  is_natural(msg_level, n = 1) && is_in_range(msg_level, min = 1, max = 10) ||
    stop("'msg_level' must be an integer between 1 and 10: ", msg_level)
  is.null(msg_types) || is.character(msg_types) ||
    stop("'msg_types' must be NULL or a character vector: ", msg_types)
  all(is_in(msg_types, c("INFO", "WARNING", "ERROR"))) ||
    stop("'msg_types' must be either 'INFO', 'WARNING' or 'ERROR': ", msg_types)
  is.character(log_path) && length(log_path) == 1 ||
    stop("'log_path' must be a string: ", log_path)

  if (!all(is_url(x))) {
    prefix <- ""
    if (sys.nframe() > 1) {
      calling_function <- deparse(sys.calls()[[sys.nframe() - 1]][[1]])
      prefix <- paste0("In ", calling_function, "(): ")
    }

    error(
      prefix,
      paste0("'", deparse(substitute(x)), "' must be a valid URL"),
      level     = level,
      msg_level = msg_level,
      msg_types = msg_types,
      log_path  = log_path
    )
  }

  invisible(TRUE)
}
