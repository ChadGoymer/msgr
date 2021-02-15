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

  # Set default values for environment variables, if they have not been set

  env <- list(
    MSGR_LEVEL    = "1",
    MSGR_TYPES    = "INFO|WARNING|ERROR",
    MSGR_LOG_PATH = ""
  )

  toset <- sapply(names(env), function(e) identical(Sys.getenv(e), ""))
  if (any(toset)) do.call(Sys.setenv, env[toset])

  # Set package options from the environment variables

  msgr_env <- as.list(Sys.getenv(names(env)))

  types <- strsplit(msgr_env[["MSGR_TYPES"]], split = "\\|")[[1]]
  if (identical(types, "NULL")) {
    types <- NULL
  }

  options(
    msgr.level    = suppressWarnings(as.integer(msgr_env[["MSGR_LEVEL"]])),
    msgr.types    = types,
    msgr.log_path = normalizePath(
      msgr_env[["MSGR_LOG_PATH"]], winslash = "/", mustWork = FALSE
    )
  )

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
