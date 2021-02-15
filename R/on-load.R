# FUNCTION: .onLoad ------------------------------------------------------------
#
# Set the package options from environment variables, using default values if
# they do not exist
#
# +---------------+---------------+-----------------------+--------------------+
# | Environment   | Option        | Description           | Default            |
# |---------------|---------------|-----------------------|--------------------|
# | MSGR_LEVEL    | msgr.level    | The level of messages | 1                  |
# |               |               |   to display          |                    |
# | MSGR_TYPES    | msgr.types    | The types of messages | INFO|WARNING|ERROR |
# |               |               |   to display          |                    |
# | MSGR_LOG_PATH | msgr.log_path | The log file path     | <none>             |
# +---------------+---------------+-----------------------+--------------------+
#
.onLoad <- function(libname, pkgname) {

  # Set message level

  if (is.null(getOption("msgr.level"))) {
    if (Sys.getenv("MSGR_LEVEL") == "") {
      level <- 1L
    } else {
      level <- suppressWarnings(as.integer(Sys.getenv("MSGR_LEVEL")))
    }
    options(msgr.level = level)
  }

  # Set message types

  if (is.null(getOption("msgr.types"))) {
    if (Sys.getenv("MSGR_TYPES") == "") {
      types <- c("INFO", "WARNING", "ERROR")
    } else {
      types <- strsplit(Sys.getenv("MSGR_TYPES"), split = "\\|")[[1]]
      if (identical(types, "NULL")) {
        types <- NULL
      }
    }
    options(msgr.types = types)
  }

  # Set log file path

  if (is.null(getOption("msgr.log_path"))) {
    if (Sys.getenv("MSGR_LOG_PATH") == "") {
      log_path <- ""
    } else {
      log_path <- normalizePath(
        Sys.getenv("MSGR_LOG_PATH"),
        winslash = "/",
        mustWork = FALSE
      )
    }
    options(msgr.log_path = log_path)
  }

}

# FUNCTION: .onAttach ----------------------------------------------------------
#
# Validate the package options
#
# msgr.level:
# - Must be a numeric vector of length 1
# - Must be an integer
# - Must be between 1 and 10
#
# msgr.types:
# - Must be a character vector
# - All elements must be either "INFO", "WARNING" or "ERROR"
#
# msgr.log_path:
# - Must be a character vector of length 1
# - If the file exists, it must be writeable
# - If the directory exists, it must be writeable
#
.onAttach <- function(libname, pkgname) {

  # Check message level

  msg_level <- getOption("msgr.level")

  if (is.na(msg_level) || !rlang::is_scalar_integerish(msg_level)) {
    packageStartupMessage(
      "The option 'msgr.level' must be an integer:\n  ", msg_level
    )
  }
  else if (msg_level < 1 || msg_level > 10) {
    packageStartupMessage(
      "The option 'msgr.level' must be between 1 and 10:\n  ", msg_level
    )
  }

  # Check message types

  msg_types <- getOption("msgr.types")

  if (!is.character(msg_types)) {
    packageStartupMessage(
      "The option 'msgr.types' must be a character vector:\n  ", msg_types
    )
  }
  else if (!all(msg_types %in% c("INFO", "WARNING", "ERROR"))) {
    packageStartupMessage(
      "The option 'msgr.types' must be either 'INFO', 'WARNING' or 'ERROR':",
      "\n  ", msg_types
    )
  }

  # Check message log path

  msg_log_path <- getOption("msgr.log_path")

  if (!is.character(msg_log_path) || !identical(length(msg_log_path), 1L)) {
    packageStartupMessage(
      "The option 'msgr.log_path' must be a string:\n  ", msg_log_path
    )
  }
  else if (
    file.exists(msg_log_path) &&
    !file.access(msg_log_path, mode = 2)[[1]] == 0
  ) {
    packageStartupMessage(
      "The option 'msgr.log_path' must be a writeable path:\n  ", msg_log_path
    )
  }
  else if (
    dir.exists(dirname(msg_log_path)) &&
    !file.access(dirname(msg_log_path), mode = 2)[[1]] == 0
  ) {
    packageStartupMessage(
      "The option 'msgr.log_path' must in a writeable directory:\n  ",
      msg_log_path
    )
  }

}
