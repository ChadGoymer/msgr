#  FUNCTION: info -----------------------------------------------------------------------------
#
#' Display a message, and record it in a log file.
#'
#' `info()` is similar to [base::message()], but it also writes the message to a log file.
#' Whether it is shown, or written to the log, depends on the level and type of the message.
#' See details below for more information.
#'
#' Whether a message is shown, or written to the log, depends on two options:
#'
#' 1. **Level**: This allows control over the depth of messages. Each message can be assigned a
#'    `level` and if it is below the `msg_level` (set in the package option `msgr.level` by
#'    default) the message is displayed and written to the log.
#' 2. **Type**: The type is refers to whether the message is "INFO", "WARNING" or "ERROR", as
#'    produced by the functions [info()], `warn()` and `error()` respectively. If the message
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
  {
    (is_natural(level) && level <= 10) ||
      stop("'level' must be an integer between 1 and 10: ", level)
    (is_natural(msg_level) && msg_level <= 10) ||
      stop("'msg_level' must be an integer between 1 and 10: ", msg_level)
    ((is_null(msg_types) || is_character(msg_types)) && is_in(msg_types, c("INFO", "WARNING", "ERROR"))) ||
      stop("'msg_types' must be NULL (no messages) or a character vector containing \"INFO\", \"WARNING\" or \"ERROR\": ", msg_types)
    is_string(log_path) ||
      stop("'log_path' must be a string: ", log_path)
  }

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
