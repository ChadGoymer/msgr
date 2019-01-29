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
  msgr_options$MSGR_LEVEL <- as.integer(msgr_options$MSGR_LEVEL)
  msgr_options$MSGR_TYPES <- strsplit(msgr_options$MSGR_TYPES, split = "\\|")[[1]]

  names(msgr_options) <- tolower(sub("_", "\\.", names(env)))
  do.call(options, msgr_options)

}
