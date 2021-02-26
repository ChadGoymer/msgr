#  FUNCTION: info --------------------------------------------------------------
#
#' Display a message, and record it in a log file.
#'
#' `info()` is similar to [message()], but it also writes the message to a log
#' file. Whether it is shown, or written to the log, depends on the level and
#' type of the message. See details below for more information.
#'
#' Whether a message is shown, or written to the log, depends on two options:
#'
#' 1. **Level**: This allows control over the depth of messages. Each message
#'    can be assigned a `level` and if it is below the `msg_level` (set in the
#'    package option `msgr.level` by default) the message is displayed and
#'    written to the log.
#' 2. **Type**: The type is refers to whether the message is "INFO", "WARNING"
#'    or "ERROR", as produced by the functions [info()], [warn()] and [error()]
#'    respectively. If the message type is in the `msg_types` (set in the
#'    package option `msgr.types` by default) the message is displayed and
#'    written to the log. This allows you to for instance, just display errors
#'    and warnings and ignore messages.
#'
#' The location of the log file is set in the package option `msgr.log_path`, or
#' as an argument to this function. messages are added with a time stamp. If the
#' `log_path` is equal to "" then no log is produced.
#'
#' @param ... (strings) message to be displayed or written to file.
#' @param level (integer, optional) The level of the message, from 1 to 10.
#'   Default: 1.
#' @param msg_level (integer, optional) The maximum level of messages to output.
#'   Default: set in the option `"msgr.level"`.
#' @param msg_types (character, optional) The type to write or display. Must
#'   either NULL or one or more from "INFO", "WARNING" or "ERROR". Default: set
#'   in the option `"msgr.types"`.
#' @param log_path (string, optional) The file path to the text log file. If set
#'   to "", then no logs are written. Default: set in the option
#'   `"msgr.log_path"`.
#'
#' @return A string is return invisibly containing the message.
#'
#' @examples
#' # Use info() to create timed messages
#' info("This is a simple message")
#' info("This is a level 2 message, so not shown by default", level = 2)
#'
#' # Set default level in options to determine what is shown
#' options(msgr.level = 2)
#' info("This is a level 2 message, so is shown now", level = 2)
#'
#' # Set message types in options to determine what is shown
#' options(msgr.types = c("WARNING", "ERROR"))
#' info("This is message, so will not be shown now")
#'
#' @export
#'
info <- function(
  ...,
  level     = 1,
  msg_level = getOption("msgr.level"),
  msg_types = getOption("msgr.types"),
  log_path  = getOption("msgr.log_path")
) {
  (rlang::is_scalar_integerish(level) && level > 0 && level <= 10) ||
    stop("'level' must be an integer between 1 and 10: ", level)
  (rlang::is_scalar_integerish(msg_level) && msg_level > 0 && msg_level <= 10) ||
    stop("'msg_level' must be an integer between 1 and 10: ", msg_level)
  (
    (is_null(msg_types) || is_character(msg_types)) &&
      all(is_in(msg_types, c("INFO", "WARNING", "ERROR")))
  ) ||
    stop(
      "'msg_types' must be NULL (no messages) or a character vector containing",
      " \"INFO\", \"WARNING\" or \"ERROR\": ", msg_types
    )
  (is_scalar_character(log_path)) ||
    stop("'log_path' must be a string: ", log_path)

  msg <- paste0(...)

  if ("INFO" %in% msg_types && level <= msg_level) {
    if (!identical(log_path, "")) {
      log_path <- normalizePath(log_path, winslash = "/", mustWork = FALSE)

      if (!is_dir(dirname(log_path))) {
        dir.create(dirname(log_path), recursive = TRUE) ||
          stop(
            "Cannot create directory for log file: '", dirname(log_path), "'"
          )
      }

      if (!is_file(log_path)) {
        file.create(log_path) ||
          stop("Cannot create log file: '", log_path, "'")
      }

      write(paste0(Sys.time(), " | INFO | ", msg), log_path, append = TRUE)
    }

    message(paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", msg))
  }

  invisible(msg)
}

#  FUNCTION: warn --------------------------------------------------------------
#
#' Display a warning, and record it in a log file.
#'
#' `warn()` is similar to [warning()], but it also writes the warning to a log
#' file. Whether it is shown, or written to the log, depends on the level and
#' type of the warning. See details below for more information.
#'
#' Whether a warning is shown, or written to the log, depends on two options:
#'
#' 1. **Level**: This allows control over the depth of messages. Each message
#'    can be assigned a `level` and if it is below the `msg_level` (set in the
#'    package option `msgr.level` by default) the message is displayed and
#'    written to the log.
#' 2. **Type**: The type is refers to whether the message is "INFO", "WARNING"
#'    or "ERROR", as produced by the functions [info()], [warn()] and [error()]
#'    respectively. If the message type is in the `msg_types` (set in the
#'    package option `msgr.types` by default) the message is displayed and
#'    written to the log. This allows you to for instance, just display errors
#'    and warnings and ignore messages.
#'
#' The location of the log file is set in the package option `msgr.log_path`, or
#' as an argument to this function. messages are added with a time stamp. If the
#' `log_path` is equal to "" then no log is produced.
#'
#' @param ... (strings) warning to be displayed or written to file.
#' @param level (integer, optional) The level of the warning, from 1 to 10.
#'   Default: 1.
#' @param msg_level (integer, optional) The maximum level of messages to output.
#'   Default: set in the option `"msgr.level"`.
#' @param msg_types (character, optional) The type to write or display. Must
#'   either NULL or one or more from "INFO", "WARNING" or "ERROR". Default: set
#'   in the option `"msgr.types"`.
#' @param log_path (string, optional) The file path to the text log file. If set
#'   to "", then no logs are written. Default: set in the option
#'   `"msgr.log_path"`.
#'
#' @return A string is return invisibly containing the warning
#'
#' @examples
#' # Use warn() to create timed warnings
#' warn("This is a simple warning")
#' warn("This is a level 2 warning, so not shown by default", level = 2)
#'
#' # Set default level in options to determine what is shown
#' options(msgr.level = 2)
#' warn("This is a level 2 warning, so is shown now", level = 2)
#'
#' # Set message types in options to determine what is shown
#' options(msgr.types = "ERROR")
#' warn("This is warning, so will not be shown now")
#'
#' @export
#'
warn <- function(
  ...,
  level     = 1,
  msg_level = getOption("msgr.level"),
  msg_types = getOption("msgr.types"),
  log_path  = getOption("msgr.log_path")
) {
  (rlang::is_scalar_integerish(level) && level > 0 && level <= 10) ||
    stop("'level' must be an integer between 1 and 10: ", level)
  (rlang::is_scalar_integerish(msg_level) && msg_level > 0 && msg_level <= 10) ||
    stop("'msg_level' must be an integer between 1 and 10: ", msg_level)
  (
    (is_null(msg_types) || is_character(msg_types)) &&
      all(is_in(msg_types, c("INFO", "WARNING", "ERROR")))
  ) ||
    stop(
      "'msg_types' must be NULL (no messages) or a character vector containing",
      " \"INFO\", \"WARNING\" or \"ERROR\": ", msg_types
    )
  (is_scalar_character(log_path)) ||
    stop("'log_path' must be a string: ", log_path)

  msg <- paste0(...)

  if ("WARNING" %in% msg_types && level <= msg_level) {
    if (!identical(log_path, "")) {
      log_path <- normalizePath(log_path, winslash = "/", mustWork = FALSE)

      if (!is_dir(dirname(log_path))) {
        dir.create(dirname(log_path), recursive = TRUE) ||
          stop(
            "Cannot create directory for log file: '", dirname(log_path), "'"
          )
      }

      if (!is_file(log_path)) {
        file.create(log_path) ||
          stop("Cannot create log file: '", log_path, "'")
      }

      write(paste0(Sys.time(), " | WARNING | ", msg), log_path, append = TRUE)
    }

    warning(
      paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", msg),
      call.      = FALSE,
      immediate. = TRUE
    )
  }

  invisible(msg)
}

#  FUNCTION: error -------------------------------------------------------------
#
#' Display an error, and record it in a log file.
#'
#' `error()` is similar to [stop()], but it also writes the error to a log file.
#' Whether it is shown, or written to the log, depends on the level and type of
#' the error. See details below for more information.
#'
#' Whether an error is shown, or written to the log, depends on two options:
#'
#' 1. **Level**: This allows control over the depth of messages. Each message
#'    can be assigned a `level` and if it is below the `msg_level` (set in the
#'    package option `msgr.level` by default) the message is displayed and
#'    written to the log.
#' 2. **Type**: The type is refers to whether the message is "INFO", "WARNING"
#'    or "ERROR", as produced by the functions [info()], [warn()] and [error()]
#'    respectively. If the message type is in the `msg_types` (set in the
#'    package option `msgr.types` by default) the message is displayed and
#'    written to the log. This allows you to for instance, just display errors
#'    and warnings and ignore messages.
#'
#' The location of the log file is set in the package option `msgr.log_path`, or
#' as an argument to this function. messages are added with a time stamp. If the
#' `log_path` is equal to "" then no log is produced.
#'
#' @param ... (strings) error to be displayed or written to file.
#' @param level (integer, optional) The level of the error, from 1 to 10.
#'   Default: 1.
#' @param msg_level (integer, optional) The maximum level of messages to output.
#'   Default: set in the option `"msgr.level"`.
#' @param msg_types (character, optional) The type to write or display. Must
#'   either NULL or one or more from "INFO", "WARNING" or "ERROR". Default: set
#'   in the option `"msgr.types"`.
#' @param log_path (string, optional) The file path to the text log file. If set
#'   to "", then no logs are written. Default: set in the option
#'   `"msgr.log_path"`.
#'
#' @return A string is return invisibly containing the error
#'
#' @examples \dontrun{
#'
#'   # Use error() to create timed errors
#'   error("This is a simple error")
#'   error("This is a level 2 error, so not shown by default", level = 2)
#'
#'   # Set default level in options to determine what is shown
#'   options(msgr.level = 2)
#'   error("This is a level 2 error, so is shown now", level = 2)
#'
#' }
#'
#' @export
#'
error <- function(
  ...,
  level     = 1,
  msg_level = getOption("msgr.level"),
  msg_types = getOption("msgr.types"),
  log_path  = getOption("msgr.log_path")
) {
  (rlang::is_scalar_integerish(level) && level > 0 && level <= 10) ||
    stop("'level' must be an integer between 1 and 10: ", level)
  (rlang::is_scalar_integerish(msg_level) && msg_level > 0 && msg_level <= 10) ||
    stop("'msg_level' must be an integer between 1 and 10: ", msg_level)
  (
    (is_null(msg_types) || is_character(msg_types)) &&
      all(is_in(msg_types, c("INFO", "WARNING", "ERROR")))
  ) ||
    stop(
      "'msg_types' must be NULL (no messages) or a character vector containing",
      " \"INFO\", \"WARNING\" or \"ERROR\": ", msg_types
    )
  (is_scalar_character(log_path)) ||
    stop("'log_path' must be a string: ", log_path)

  msg <- paste0(...)

  if ("ERROR" %in% msg_types && level <= msg_level) {
    if (!identical(log_path, "")) {
      log_path <- normalizePath(log_path, winslash = "/", mustWork = FALSE)

      if (!is_dir(dirname(log_path))) {
        dir.create(dirname(log_path), recursive = TRUE) ||
          stop(
            "Cannot create directory for log file: '", dirname(log_path), "'"
          )
      }

      if (!is_file(log_path)) {
        file.create(log_path) ||
          stop("Cannot create log file: '", log_path, "'")
      }

      write(paste0(Sys.time(), " | ERROR | ", msg), log_path, append = TRUE)
    }

    stop(paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", msg), call. = FALSE)
  }

  invisible(msg)
}

# FUNCTION: info_if ------------------------------------------------------------
#
#' Display a message, and record in a log file, if a condition is true.
#'
#' This function calls the [info()] function to display a message if the
#' specified condition is true. If a message is not specified then a generic
#' message is displayed.
#'
#' @param condition (boolean) The condition to check.
#' @param ... (strings) message to be displayed or written to file.
#' @param level (integer, optional) The level of the message, from 1 to 10.
#'   Default: 1.
#' @param msg_level (integer, optional) The maximum level of messages to output.
#'   Default: set in the option `"msgr.level"`.
#' @param msg_types (character, optional) The type to write or display. Must
#'   either NULL or one or more from "INFO", "WARNING" or "ERROR". Default: set
#'   in the option `"msgr.types"`.
#' @param log_path (string, optional) The file path to the text log file. If set
#'   to "", then no logs are written. Default: set in the option
#'   `"msgr.log_path"`.
#'
#' @return A string is return invisibly containing the message.
#'
#' @examples
#' # Use info_if() to create conditional timed messages
#' info_if(2 > 1, "Condition is true so this message is shown")
#' info_if(1 > 2, "Condition is false so this message is not shown")
#'
#' # As with info() a level can be set
#' info_if(2 > 1, "This is a level 2 message, so not shown", level = 2)
#'
#' # Set default level in options to determine what is shown
#' options(msgr.level = 2)
#' info_if(2 > 1, "This is a level 2 message, so is shown now", level = 2)
#'
#' # Set message types in options to determine what is shown
#' options(msgr.types = c("WARNING", "ERROR"))
#' info_if(2 > 1, "This is message, so will not be shown now")
#'
#' @export
#'
info_if <- function(
  condition,
  ...,
  level     = 1,
  msg_level = getOption("msgr.level"),
  msg_types = getOption("msgr.types"),
  log_path  = getOption("msgr.log_path")
) {
  (rlang::is_scalar_integerish(level) && level > 0 && level <= 10) ||
    stop("'level' must be an integer between 1 and 10: ", level)
  (rlang::is_scalar_integerish(msg_level) && msg_level > 0 && msg_level <= 10) ||
    stop("'msg_level' must be an integer between 1 and 10: ", msg_level)
  (
    (is_null(msg_types) || is_character(msg_types)) &&
      all(is_in(msg_types, c("INFO", "WARNING", "ERROR")))
  ) ||
    stop(
      "'msg_types' must be NULL (no messages) or a character vector containing",
      " \"INFO\", \"WARNING\" or \"ERROR\": ", msg_types
    )
  (is_scalar_character(log_path)) ||
    stop("'log_path' must be a string: ", log_path)

  if (isTRUE(condition)) {
    uneval_condition <- substitute(condition)

    if (sys.nframe() > 1) {
      calling_function <- deparse(sys.calls()[[sys.nframe() - 1]][[1]])
      prefix <- paste0("In ", calling_function, "(): ")
    } else {
      prefix <- ""
    }

    msg <- list(...)
    if (identical(length(msg), 0L)) {
      msg <- paste(deparse(uneval_condition), "is true")
    } else {
      msg <- paste(as.character(msg), collapse = "")
    }

    info(
      prefix, msg,
      level     = level,
      msg_level = msg_level,
      msg_types = msg_types,
      log_path  = log_path
    )
  }
}

# FUNCTION: warn_if ------------------------------------------------------------
#
#' Display a warning, and record in a log file, if a condition is true.
#'
#' This function calls the [warn()] function to display a warning if the
#' specified condition is true. If a message is not specified then a generic
#' message is displayed.
#'
#' @param condition (boolean) The condition to check.
#' @param ... (strings) message to be displayed or written to file.
#' @param level (integer, optional) The level of the message, from 1 to 10.
#'   Default: 1.
#' @param msg_level (integer, optional) The maximum level of messages to output.
#'   Default: set in the option `"msgr.level"`.
#' @param msg_types (character, optional) The type to write or display. Must
#'   either NULL or one or more from "INFO", "WARNING" or "ERROR". Default: set
#'   in the option `"msgr.types"`.
#' @param log_path (string, optional) The file path to the text log file. If set
#'   to "", then no logs are written. Default: set in the option
#'   `"msgr.log_path"`.
#'
#' @return A string is return invisibly containing the warning.
#'
#' @examples
#' # Use warn_if() to create conditional timed warnings
#' warn_if(2 > 1, "Condition is true so this warning is shown")
#' warn_if(1 > 2, "Condition is false so this warning is not shown")
#'
#' # As with warn() a level can be set
#' warn_if(2 > 1, "This level 2 warning is not shown by default", level = 2)
#'
#' # Set default level in options to determine what is shown
#' options(msgr.level = 2)
#' warn_if(2 > 1, "This level 2 warning is shown now", level = 2)
#'
#' # Set message types in options to determine what is shown
#' options(msgr.types = "ERROR")
#' warn_if(2 > 1, "This is warning, so will not be shown now")
#'
#' @export
#'
warn_if <- function(
  condition,
  ...,
  level     = 1,
  msg_level = getOption("msgr.level"),
  msg_types = getOption("msgr.types"),
  log_path  = getOption("msgr.log_path")
) {
  (rlang::is_scalar_integerish(level) && isTRUE(level > 0) && isTRUE(level <= 10)) ||
    error("'level' must be an integer between 1 and 10: ", level)
  (
    rlang::is_scalar_integerish(msg_level) &&
      isTRUE(msg_level > 0) &&
      isTRUE(msg_level <= 10)
  ) ||
    error("'msg_level' must be an integer between 1 and 10: ", msg_level)
  (
    (is_null(msg_types) || is_character(msg_types)) &&
      all(is_in(msg_types, c("INFO", "WARNING", "ERROR")))
  ) ||
    error(
      "'msg_types' must be NULL (no messages) or a character vector containing",
      " \"INFO\", \"WARNING\" or \"ERROR\": ", msg_types
    )
  (is_scalar_character(log_path)) ||
    error("'log_path' must be a string: ", log_path)

  if (isTRUE(condition)) {
    uneval_condition <- substitute(condition)

    if (sys.nframe() > 1) {
      calling_function <- deparse(sys.calls()[[sys.nframe() - 1]][[1]])
      prefix <- paste0("In ", calling_function, "(): ")
    } else {
      prefix <- ""
    }

    msg <- list(...)
    if (identical(length(msg), 0L)) {
      msg <- paste(deparse(uneval_condition), "is true")
    } else {
      msg <- paste(as.character(msg), collapse = "")
    }

    warn(
      prefix, msg,
      level     = level,
      msg_level = msg_level,
      msg_types = msg_types,
      log_path  = log_path
    )
  }
}

# FUNCTION: error_if -----------------------------------------------------------
#
#' Display an error, and record in a log file, if a condition is true.
#'
#' This function calls the [error()] function to display an error if the
#' specified condition is true. If a message is not specified then a generic
#' message is displayed.
#'
#' @param condition (boolean) The condition to check.
#' @param ... (strings) message to be displayed or written to file.
#' @param level (integer, optional) The level of the message, from 1 to 10.
#'   Default: 1.
#' @param msg_level (integer, optional) The maximum level of messages to output.
#'   Default: set in the option `"msgr.level"`.
#' @param msg_types (character, optional) The type to write or display. Must
#'   either NULL or one or more from "INFO", "WARNING" or "ERROR". Default: set
#'   in the option `"msgr.types"`.
#' @param log_path (string, optional) The file path to the text log file. If set
#'   to "", then no logs are written. Default: set in the option
#'   `"msgr.log_path"`.
#'
#' @return A string is return invisibly containing the error
#'
#' @examples \dontrun{
#'
#'   # Use error_if() to create conditional timed errors
#'   error_if(2 > 1, "Condition is true so this error is shown")
#'   error_if(1 > 2, "Condition is false so this error is not shown")
#'
#'   # As with error() a level can be set
#'   error_if(2 > 1, "This level 2 error is not shown by default", level = 2)
#'
#'   # Set default level in options to determine what is shown
#'   options(msgr.level = 2)
#'   error_if(2 > 1, "This is a level 2 error, so is shown now", level = 2)
#'
#' }
#'
#' @export
#'
error_if <- function(
  condition,
  ...,
  level     = 1,
  msg_level = getOption("msgr.level"),
  msg_types = getOption("msgr.types"),
  log_path  = getOption("msgr.log_path")
) {
  (rlang::is_scalar_integerish(level) && isTRUE(level > 0) && isTRUE(level <= 10)) ||
    error("'level' must be an integer between 1 and 10: ", level)
  (
    rlang::is_scalar_integerish(msg_level) &&
      isTRUE(msg_level > 0) &&
      isTRUE(msg_level <= 10)
  ) ||
    error("'msg_level' must be an integer between 1 and 10: ", msg_level)
  (
    (is_null(msg_types) || is_character(msg_types)) &&
      all(is_in(msg_types, c("INFO", "WARNING", "ERROR")))
  ) ||
    error(
      "'msg_types' must be NULL (no messages) or a character vector containing",
      " \"INFO\", \"WARNING\" or \"ERROR\": ", msg_types
    )
  (is_scalar_character(log_path)) ||
    error("'log_path' must be a string: ", log_path)

  if (isTRUE(condition)) {
    uneval_condition <- substitute(condition)

    if (sys.nframe() > 1) {
      calling_function <- deparse(sys.calls()[[sys.nframe() - 1]][[1]])
      prefix <- paste0("In ", calling_function, "(): ")
    } else {
      prefix <- ""
    }

    msg <- list(...)
    if (identical(length(msg), 0L)) {
      msg <- paste(deparse(uneval_condition), "is true")
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
}

#  FUNCTION: assert ------------------------------------------------------------
#
#' Display an error, and record in a log file, if a condition is false
#'
#' This function calls the [error()] function to display an error if the
#' specified condition is false. If a message is not specified then a generic
#' message is displayed.
#'
#' @param condition (boolean) The condition to check.
#' @param ... (strings) message to be displayed or written to file.
#' @param level (integer, optional) The level of the message, from 1 to 10.
#'   Default: 1.
#' @param msg_level (integer, optional) The maximum level of messages to output.
#'   Default: set in the option `"msgr.level"`.
#' @param msg_types (character, optional) The type to write or display. Must
#'   either NULL or one or more from "INFO", "WARNING" or "ERROR". Default: set
#'   in the option `"msgr.types"`.
#' @param log_path (string, optional) The file path to the text log file. If set
#'   to "", then no logs are written. Default: set in the option
#'   `"msgr.log_path"`.
#'
#' @return A string is return invisibly containing the error
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
  (
    is_scalar_integerish(level) &&
      isTRUE(level > 0) &&
      isTRUE(level <= 10)
  ) ||
    error("'level' must be an integer between 1 and 10: ", level)
  (
    is_scalar_integerish(msg_level) &&
      isTRUE(msg_level > 0) &&
      isTRUE(msg_level <= 10)
  ) ||
    error("'msg_level' must be an integer between 1 and 10: ", msg_level)
  (
    (is_null(msg_types) || is_character(msg_types)) &&
      all(is_in(msg_types, c("INFO", "WARNING", "ERROR")))
  ) ||
    error(
      "'msg_types' must be NULL (no messages) or a character vector containing",
      " \"INFO\", \"WARNING\" or \"ERROR\": ", msg_types
    )
  (is_scalar_character(log_path)) ||
    error("'log_path' must be a string: ", log_path)

  if (!isTRUE(condition)) {
    uneval_condition <- substitute(condition)

    if (sys.nframe() > 1) {
      calling_function <- deparse(sys.calls()[[sys.nframe() - 1]][[1]])
      prefix <- paste0("In ", calling_function, "(): ")
    } else {
      prefix <- ""
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
}
