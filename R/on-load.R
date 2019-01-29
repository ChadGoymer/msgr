# FUNCTION: .onLoad ---------------------------------------------------------------------------
#
# Set the package options from environment variables, using default values if they do not exist
#
# +---------------+---------------+----------------------------------+-------------------------+
# | Environment   | Option        | Description                      | Default                 |
# |---------------|---------------|----------------------------------|-------------------------|
# | MSGR_LEVEL    | msgr.level    | The level of messages to display | 1                       |
# | MSGR_TYPES    | msgr.types    | The types of messages to display | INFO, WARNING and ERROR |
# | MSGR_LOG_PATH | msgr.log_path | The log file path                | msgr.msg                |
# +---------------+---------------+----------------------------------+-------------------------+
#
.onLoad <- function(libname, pkgname) {

  # Set default values for environment variables, if they have not been set

  env <- list(
    MSGR_LEVEL    = "1",
    MSGR_TYPES    = "INFO|WARNING|ERROR",
    MSGR_LOG_PATH = ""
  )

  toset <- sapply(names(env), function(e) identical(Sys.getenv(e), ""))
  if (any(toset)) do.call(Sys.setenv, env[toset])

  # Set package options from the environment variables

  msgr_options <- as.list(Sys.getenv(names(env)))
  names(msgr_options) <- tolower(sub("_", "\\.", names(env)))
  do.call(options, msgr_options)

}

# FUNCTION: .onAttach -------------------------------------------------------------------------
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

  msg_level <- suppressWarnings(as.integer(getOption("msgr.level")))

  if (is.na(msg_level) || !identical(length(msg_level), 1L)) {
    packageStartupMessage("The message level defined in the option 'msgr.level' must be an integer:\n  ", getOption("msgr.level"))
  }
  else if (msg_level < 1 || msg_level > 10) {
    packageStartupMessage("The message level defined in the option 'msgr.level' must be between 1 and 10:\n  ", getOption("msgr.level"))
  }

  options(msgr.level = msg_level)

  # Check message types

  msg_types <- strsplit(getOption("msgr.types"), split = "\\|")[[1]]

  if (!is.character(msg_types)) {
    packageStartupMessage("The message types defined in the option 'msgr.types' must be a character vector:\n  ", getOption("msgr.types"))
  }
  else if (!all(msg_types %in% c("INFO", "WARNING", "ERROR"))) {
    packageStartupMessage("The message types defined in the option 'msgr.types' must be either 'INFO', 'WARNING' or 'ERROR':\n  ", getOption("msgr.types"))
  }

  options(msgr.types = msg_types)

  # Check message log path

  msg_log_path <- normalizePath(getOption("msgr.log_path"), winslash = "/", mustWork = FALSE)

  if (!is.character(msg_log_path) || !identical(length(msg_log_path), 1L)) {
    packageStartupMessage("The message path defined in the option 'msgr.log_path' must be a string:\n  ", getOption("msgr.log_path"))
  }
  else if (file.exists(msg_log_path) && !file.access(msg_log_path, mode = 2)[[1]] == 0) {
    packageStartupMessage("The message path defined in the option 'msgr.log_path' must be a writeable:\n  ", getOption("msgr.log_path"))
  }
  else if (dir.exists(dirname(msg_log_path)) && !file.access(dirname(msg_log_path), mode = 2)[[1]] == 0) {
    packageStartupMessage("The directory containing the message file defined in the option 'msgr.log_path' must be writeable:\n  ", getOption("msgr.log_path"))
  }

  options(msgr.log_path = msg_log_path)

}
