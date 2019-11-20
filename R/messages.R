#  FUNCTION: info -----------------------------------------------------------------------------
#
#' Display a message, and record it in a log file.
#'
#' `info()` is similar to [message()], but it also writes the message to a log file.
#' Whether it is shown, or written to the log, depends on the level and type of the message.
#' See details below for more information.
#'
#' Whether a message is shown, or written to the log, depends on two options:
#'
#' 1. **Level**: This allows control over the depth of messages. Each message can be assigned a
#'    `level` and if it is below the `msg_level` (set in the package option `msgr.level` by
#'    default) the message is displayed and written to the log.
#' 2. **Type**: The type is refers to whether the message is "INFO", "WARNING" or "ERROR", as
#'    produced by the functions [info()], [warn()] and [error()] respectively. If the message
#'    type is in the `msg_types` (set in the package option `msgr.types` by default) the
#'    message is displayed and written to the log. This allows you to for instance, just
#'    display errors and warnings and ignore messages.
#'
#' The location of the log file is set in the package option `msgr.log_path`, or as an argument to
#' this function. messages are added with a time stamp. If the `log_path` is equal to "" then no
#' log is produced.
#'
#' @param ... (strings) message to be displayed or written to file.
#' @param level (integer, optional) The level of the message, from 1 to 10. Default: 1.
#' @param msg_level (integer, optional) The maximum level of messages to output. Default: set
#'   in the option `"msgr.level"`.
#' @param msg_types (character, optional) The type to write or display. Must either NULL or one
#'  or more from "INFO", "WARNING" or "ERROR". Default: set in the option `"msgr.types"`.
#' @param log_path (string, optional) The file path to the text log file. If set to "", then no
#'   logs are written. Default: set in the option `"msgr.log_path"`.
#'
#' @return A string is return invisibly containing the message.
#'
#' @export
#'
info <- function(
  ...,
  level     = 1,
  msg_level = getOption("msgr.level"),
  msg_types = getOption("msgr.types"),
  log_path  = getOption("msgr.log_path"))
{
  (is_scalar_integerish(level) && isTRUE(level > 0) && isTRUE(level <= 10)) ||
    stop("'level' must be an integer between 1 and 10: ", level)
  (is_scalar_integerish(msg_level) && isTRUE(msg_level > 0) && isTRUE(msg_level <= 10)) ||
    stop("'msg_level' must be an integer between 1 and 10: ", msg_level)
  ((is_null(msg_types) || is_character(msg_types)) && is_in(msg_types, c("INFO", "WARNING", "ERROR"))) ||
    stop("'msg_types' must be NULL (no messages) or a character vector containing \"INFO\", \"WARNING\" or \"ERROR\": ", msg_types)
  (is_scalar_character(log_path)) ||
    stop("'log_path' must be a string: ", log_path)

  msg <- paste0(...)

  if ("INFO" %in% msg_types && level <= msg_level) {
    if (!identical(log_path, "")) {
      log_path <- normalizePath(log_path, winslash = "/", mustWork = FALSE)

      if (!is_dir(dirname(log_path))) {
        dir.create(dirname(log_path), recursive = TRUE) ||
          stop("Cannot create directory for log file: '", dirname(log_path), "'")
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

#  FUNCTION: warn -----------------------------------------------------------------------------
#
#' Display a warning, and record it in a log file.
#'
#' `warn()` is similar to [warning()], but it also writes the warning to a log file.
#' Whether it is shown, or written to the log, depends on the level and type of the warning.
#' See details below for more information.
#'
#' Whether a warning is shown, or written to the log, depends on two options:
#'
#' 1. **Level**: This allows control over the depth of messages. Each message can be assigned a
#'    `level` and if it is below the `msg_level` (set in the package option `msgr.level` by
#'    default) the message is displayed and written to the log.
#' 2. **Type**: The type is refers to whether the message is "INFO", "WARNING" or "ERROR", as
#'    produced by the functions [info()], [warn()] and [error()] respectively. If the message
#'    type is in the `msg_types` (set in the package option `msgr.types` by default) the
#'    message is displayed and written to the log. This allows you to for instance, just
#'    display errors and warnings and ignore messages.
#'
#' The location of the log file is set in the package option `msgr.log_path`, or as an argument to
#' this function. messages are added with a time stamp. If the `log_path` is equal to "" then no
#' log is produced.
#'
#' @param ... (strings) warning to be displayed or written to file.
#' @param level (integer, optional) The level of the warning, from 1 to 10. Default: 1.
#' @param msg_level (integer, optional) The maximum level of messages to output. Default: set
#'   in the option `"msgr.level"`.
#' @param msg_types (character, optional) The type to write or display. Must either NULL or one
#'  or more from "INFO", "WARNING" or "ERROR". Default: set in the option `"msgr.types"`.
#' @param log_path (string, optional) The file path to the text log file. If set to "", then no
#'   logs are written. Default: set in the option `"msgr.log_path"`.
#'
#' @return A string is return invisibly containing the warning
#'
#' @export
#'
warn <- function(
  ...,
  level     = 1,
  msg_level = getOption("msgr.level"),
  msg_types = getOption("msgr.types"),
  log_path  = getOption("msgr.log_path"))
{
  (is_scalar_integerish(level) && isTRUE(level > 0) && isTRUE(level <= 10)) ||
    stop("'level' must be an integer between 1 and 10: ", level)
  (is_scalar_integerish(msg_level) && isTRUE(msg_level > 0) && isTRUE(msg_level <= 10)) ||
    stop("'msg_level' must be an integer between 1 and 10: ", msg_level)
  ((is_null(msg_types) || is_character(msg_types)) && is_in(msg_types, c("INFO", "WARNING", "ERROR"))) ||
    stop("'msg_types' must be NULL (no messages) or a character vector containing \"INFO\", \"WARNING\" or \"ERROR\": ", msg_types)
  (is_scalar_character(log_path)) ||
    stop("'log_path' must be a string: ", log_path)

  msg <- paste0(...)

  if ("WARNING" %in% msg_types && level <= msg_level) {
    if (!identical(log_path, "")) {
      log_path <- normalizePath(log_path, winslash = "/", mustWork = FALSE)

      if (!is_dir(dirname(log_path))) {
        dir.create(dirname(log_path), recursive = TRUE) ||
          stop("Cannot create directory for log file: '", dirname(log_path), "'")
      }

      if (!is_file(log_path)) {
        file.create(log_path) ||
          stop("Cannot create log file: '", log_path, "'")
      }

      write(paste0(Sys.time(), " | WARNING | ", msg), log_path, append = TRUE)
    }

    warning(paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", msg), call. = FALSE, immediate. = TRUE)
  }

  invisible(msg)
}

#  FUNCTION: error ----------------------------------------------------------------------------
#
#' Display an error, and record it in a log file.
#'
#' `error()` is similar to [stop()], but it also writes the error to a log file.
#' Whether it is shown, or written to the log, depends on the level and type of the error.
#' See details below for more information.
#'
#' Whether an error is shown, or written to the log, depends on two options:
#'
#' 1. **Level**: This allows control over the depth of messages. Each message can be assigned a
#'    `level` and if it is below the `msg_level` (set in the package option `msgr.level` by
#'    default) the message is displayed and written to the log.
#' 2. **Type**: The type is refers to whether the message is "INFO", "WARNING" or "ERROR", as
#'    produced by the functions [info()], [warn()] and [error()] respectively. If the message
#'    type is in the `msg_types` (set in the package option `msgr.types` by default) the
#'    message is displayed and written to the log. This allows you to for instance, just
#'    display errors and warnings and ignore messages.
#'
#' The location of the log file is set in the package option `msgr.log_path`, or as an argument to
#' this function. messages are added with a time stamp. If the `log_path` is equal to "" then no
#' log is produced.
#'
#' @param ... (strings) error to be displayed or written to file.
#' @param level (integer, optional) The level of the error, from 1 to 10. Default: 1.
#' @param msg_level (integer, optional) The maximum level of messages to output. Default: set
#'   in the option `"msgr.level"`.
#' @param msg_types (character, optional) The type to write or display. Must either NULL or one
#'  or more from "INFO", "WARNING" or "ERROR". Default: set in the option `"msgr.types"`.
#' @param log_path (string, optional) The file path to the text log file. If set to "", then no
#'   logs are written. Default: set in the option `"msgr.log_path"`.
#'
#' @return A string is return invisibly containing the error
#'
#' @export
#'
error <- function(
  ...,
  level     = 1,
  msg_level = getOption("msgr.level"),
  msg_types = getOption("msgr.types"),
  log_path  = getOption("msgr.log_path"))
{
  (is_scalar_integerish(level) && isTRUE(level > 0) && isTRUE(level <= 10)) ||
    stop("'level' must be an integer between 1 and 10: ", level)
  (is_scalar_integerish(msg_level) && isTRUE(msg_level > 0) && isTRUE(msg_level <= 10)) ||
    stop("'msg_level' must be an integer between 1 and 10: ", msg_level)
  ((is_null(msg_types) || is_character(msg_types)) && is_in(msg_types, c("INFO", "WARNING", "ERROR"))) ||
    stop("'msg_types' must be NULL (no messages) or a character vector containing \"INFO\", \"WARNING\" or \"ERROR\": ", msg_types)
  (is_scalar_character(log_path)) ||
    stop("'log_path' must be a string: ", log_path)

  msg <- paste0(...)

  if ("ERROR" %in% msg_types && level <= msg_level) {
    if (!identical(log_path, "")) {
      log_path <- normalizePath(log_path, winslash = "/", mustWork = FALSE)

      if (!is_dir(dirname(log_path))) {
        dir.create(dirname(log_path), recursive = TRUE) ||
          stop("Cannot create directory for log file: '", dirname(log_path), "'")
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

# FUNCTION: info_if ---------------------------------------------------------------------------
#
#' Display a message, and record in a log file, if a condition is true.
#'
#' This function calls the [info()] function to display a message if the specified condition
#' is true. If a message is not specified then a generic message is displayed.
#'
#' @param condition (boolean) The condition to check.
#' @param ... (strings) message to be displayed or written to file.
#' @param level (integer, optional) The level of the message, from 1 to 10. Default: 1.
#' @param msg_level (integer, optional) The maximum level of messages to output. Default: set
#'   in the option `"msgr.level"`.
#' @param msg_types (character, optional) The type to write or display. Must either NULL or one
#'  or more from "INFO", "WARNING" or "ERROR". Default: set in the option `"msgr.types"`.
#' @param log_path (string, optional) The file path to the text log file. If set to "", then no
#'   logs are written. Default: set in the option `"msgr.log_path"`.
#'
#' @return A string is return invisibly containing the message.
#'
#' @export
#'
info_if <- function(
  condition,
  ...,
  level     = 1,
  msg_level = getOption("msgr.level"),
  msg_types = getOption("msgr.types"),
  log_path  = getOption("msgr.log_path"))
{
  (is_scalar_integerish(level) && isTRUE(level > 0) && isTRUE(level <= 10)) ||
    error("'level' must be an integer between 1 and 10: ", level)
  (is_scalar_integerish(msg_level) && isTRUE(msg_level > 0) && isTRUE(msg_level <= 10)) ||
    error("'msg_level' must be an integer between 1 and 10: ", msg_level)
  ((is_null(msg_types) || is_character(msg_types)) && is_in(msg_types, c("INFO", "WARNING", "ERROR"))) ||
    error("'msg_types' must be NULL (no messages) or a character vector containing \"INFO\", \"WARNING\" or \"ERROR\": ", msg_types)
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

    info(prefix, msg, level = level, msg_level = msg_level, msg_types = msg_types, log_path = log_path)
  }
}

# FUNCTION: warn_if ---------------------------------------------------------------------------
#
#' Display a warning, and record in a log file, if a condition is true.
#'
#' This function calls the [warn()] function to display a warning if the specified condition
#' is true. If a message is not specified then a generic message is displayed.
#'
#' @param condition (boolean) The condition to check.
#' @param ... (strings) message to be displayed or written to file.
#' @param level (integer, optional) The level of the message, from 1 to 10. Default: 1.
#' @param msg_level (integer, optional) The maximum level of messages to output. Default: set
#'   in the option `"msgr.level"`.
#' @param msg_types (character, optional) The type to write or display. Must either NULL or one
#'  or more from "INFO", "WARNING" or "ERROR". Default: set in the option `"msgr.types"`.
#' @param log_path (string, optional) The file path to the text log file. If set to "", then no
#'   logs are written. Default: set in the option `"msgr.log_path"`.
#'
#' @return A string is return invisibly containing the warning.
#'
#' @export
#'
warn_if <- function(
  condition,
  ...,
  level     = 1,
  msg_level = getOption("msgr.level"),
  msg_types = getOption("msgr.types"),
  log_path  = getOption("msgr.log_path"))
{
  (is_scalar_integerish(level) && isTRUE(level > 0) && isTRUE(level <= 10)) ||
    error("'level' must be an integer between 1 and 10: ", level)
  (is_scalar_integerish(msg_level) && isTRUE(msg_level > 0) && isTRUE(msg_level <= 10)) ||
    error("'msg_level' must be an integer between 1 and 10: ", msg_level)
  ((is_null(msg_types) || is_character(msg_types)) && is_in(msg_types, c("INFO", "WARNING", "ERROR"))) ||
    error("'msg_types' must be NULL (no messages) or a character vector containing \"INFO\", \"WARNING\" or \"ERROR\": ", msg_types)
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

    warn(prefix, msg, level = level, msg_level = msg_level, msg_types = msg_types, log_path = log_path)
  }
}

# FUNCTION: error_if --------------------------------------------------------------------------
#
#' Display an error, and record in a log file, if a condition is true.
#'
#' This function calls the [error()] function to display an error if the specified condition
#' is true. If a message is not specified then a generic message is displayed.
#'
#' @param condition (boolean) The condition to check.
#' @param ... (strings) message to be displayed or written to file.
#' @param level (integer, optional) The level of the message, from 1 to 10. Default: 1.
#' @param msg_level (integer, optional) The maximum level of messages to output. Default: set
#'   in the option `"msgr.level"`.
#' @param msg_types (character, optional) The type to write or display. Must either NULL or one
#'  or more from "INFO", "WARNING" or "ERROR". Default: set in the option `"msgr.types"`.
#' @param log_path (string, optional) The file path to the text log file. If set to "", then no
#'   logs are written. Default: set in the option `"msgr.log_path"`.
#'
#' @return A string is return invisibly containing the error
#'
#' @export
#'
error_if <- function(
  condition,
  ...,
  level     = 1,
  msg_level = getOption("msgr.level"),
  msg_types = getOption("msgr.types"),
  log_path  = getOption("msgr.log_path"))
{
  (is_scalar_integerish(level) && isTRUE(level > 0) && isTRUE(level <= 10)) ||
    error("'level' must be an integer between 1 and 10: ", level)
  (is_scalar_integerish(msg_level) && isTRUE(msg_level > 0) && isTRUE(msg_level <= 10)) ||
    error("'msg_level' must be an integer between 1 and 10: ", msg_level)
  ((is_null(msg_types) || is_character(msg_types)) && is_in(msg_types, c("INFO", "WARNING", "ERROR"))) ||
    error("'msg_types' must be NULL (no messages) or a character vector containing \"INFO\", \"WARNING\" or \"ERROR\": ", msg_types)
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

    error(prefix, msg, level = level, msg_level = msg_level, msg_types = msg_types, log_path = log_path)
  }
}

# FUNCTION: assert ----------------------------------------------------------------------------
#
#' Display an error, and record in a log file, if a condition is false
#'
#' This function calls the [error()] function to display an error if the specified condition
#' is false. If a message is not specified then a generic message is displayed.
#'
#' @param condition (boolean) The condition to check.
#' @param ... (strings) message to be displayed or written to file.
#' @param level (integer, optional) The level of the message, from 1 to 10. Default: 1.
#' @param msg_level (integer, optional) The maximum level of messages to output. Default: set
#'   in the option `"msgr.level"`.
#' @param msg_types (character, optional) The type to write or display. Must either NULL or one
#'  or more from "INFO", "WARNING" or "ERROR". Default: set in the option `"msgr.types"`.
#' @param log_path (string, optional) The file path to the text log file. If set to "", then no
#'   logs are written. Default: set in the option `"msgr.log_path"`.
#'
#' @return A string is return invisibly containing the error
#'
#' @export
#'
assert <- function(
  condition,
  ...,
  level     = 1,
  msg_level = getOption("msgr.level"),
  msg_types = getOption("msgr.types"),
  log_path  = getOption("msgr.log_path"))
{
  (is_scalar_integerish(level) && isTRUE(level > 0) && isTRUE(level <= 10)) ||
    error("'level' must be an integer between 1 and 10: ", level)
  (is_scalar_integerish(msg_level) && isTRUE(msg_level > 0) && isTRUE(msg_level <= 10)) ||
    error("'msg_level' must be an integer between 1 and 10: ", msg_level)
  ((is_null(msg_types) || is_character(msg_types)) && is_in(msg_types, c("INFO", "WARNING", "ERROR"))) ||
    error("'msg_types' must be NULL (no messages) or a character vector containing \"INFO\", \"WARNING\" or \"ERROR\": ", msg_types)
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

    error(prefix, msg, level = level, msg_level = msg_level, msg_types = msg_types, log_path = log_path)
  }
}
