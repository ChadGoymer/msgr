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
    if (identical(length(msg), 0L)) {
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
