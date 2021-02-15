
# TEST: .onLoad ----------------------------------------------------------------

test_that("Default options are set when package is loaded", {

  original_env_vars <- list(
    MSGR_LEVEL    = Sys.getenv("MSGR_LEVEL"),
    MSGR_TYPES    = Sys.getenv("MSGR_TYPES"),
    MSGR_LOG_PATH = Sys.getenv("MSGR_LOG_PATH")
  )

  original_options <- options(
    msgr.level    = NULL,
    msgr.types    = NULL,
    msgr.log_path = NULL
  )

  on.exit({
    do.call(Sys.setenv, original_env_vars)
    do.call(options, original_options)
    .onLoad()
  })

  Sys.setenv(MSGR_LEVEL    = "")
  Sys.setenv(MSGR_TYPES    = "")
  Sys.setenv(MSGR_LOG_PATH = "")

  .onLoad()

  expect_identical(getOption("msgr.level"), 1L)
  expect_identical(getOption("msgr.types"), c("INFO", "WARNING", "ERROR"))
  expect_identical(getOption("msgr.log_path"), "")

})

test_that("Options are overidden when environment variables are set", {

  original_env_vars <- list(
    MSGR_LEVEL    = Sys.getenv("MSGR_LEVEL"),
    MSGR_TYPES    = Sys.getenv("MSGR_TYPES"),
    MSGR_LOG_PATH = Sys.getenv("MSGR_LOG_PATH")
  )

  original_options <- options(
    msgr.level    = NULL,
    msgr.types    = NULL,
    msgr.log_path = NULL
  )

  on.exit({
    do.call(Sys.setenv, original_env_vars)
    do.call(options, original_options)
    .onLoad()
  })

  Sys.setenv(MSGR_LEVEL    = "10")
  Sys.setenv(MSGR_TYPES    = "NULL")
  Sys.setenv(MSGR_LOG_PATH = "C:/temp/test-msgr.log")

  .onLoad()

  expect_identical(getOption("msgr.level"), 10L)
  expect_null(getOption("msgr.types"))
  expect_identical(getOption("msgr.log_path"), "C:/temp/test-msgr.log")

})

test_that("Options are overidden when options are set", {

  original_env_vars <- list(
    MSGR_LEVEL    = Sys.getenv("MSGR_LEVEL"),
    MSGR_TYPES    = Sys.getenv("MSGR_TYPES"),
    MSGR_LOG_PATH = Sys.getenv("MSGR_LOG_PATH")
  )

  original_options <- options(
    msgr.level    = 5L,
    msgr.types    = "INFO",
    msgr.log_path = "C:/temp/test.log"
  )

  on.exit({
    do.call(Sys.setenv, original_env_vars)
    do.call(options, original_options)
    .onLoad()
  })

  .onLoad()

  expect_identical(getOption("msgr.level"), 5L)
  expect_identical(getOption("msgr.types"), "INFO")
  expect_identical(getOption("msgr.log_path"), "C:/temp/test.log")

})

# TEST: .onAttach --------------------------------------------------------------

test_that("Options are validation when package is attached", {

  original_options <- options(
    msgr.level    = 1,
    msgr.types    = c("INFO", "WARNING", "ERROR"),
    msgr.log_path = ""
  )

  on.exit({
    do.call(options, original_options)
    .onAttach()
  })

  # Check valid options do not raise any messages

  expect_silent(.onAttach())

  # Check invalid 'msgr.level' option displays a message

  options(msgr.level = "one")
  expect_message(.onAttach(), "The option 'msgr.level' must be an integer")

  options(msgr.level = -1)
  expect_message(
    .onAttach(),
    "The option 'msgr.level' must be between 1 and 10"
  )

  options(msgr.level = 11)
  expect_message(
    .onAttach(),
    "The option 'msgr.level' must be between 1 and 10"
  )

  options(msgr.level = original_options$msgr.level)

  # Check invalid 'msgr.types' option displays a message

  options(msgr.types = 1)
  expect_message(
    .onAttach(),
    "The option 'msgr.types' must be a character vector"
  )

  options(msgr.types = c("INFO", "INVALID"))
  expect_message(
    .onAttach(),
    "The option 'msgr.types' must be either 'INFO', 'WARNING' or 'ERROR'"
  )

  options(msgr.types = original_options$msgr.types)

  # Check invalid 'msgr.log_path' option displays a message

  options(msgr.log_path = 1)
  expect_message(
    .onAttach(),
    "The option 'msgr.log_path' must be a string"
  )

  options(msgr.log_path = original_options$msgr.log_path)

})
