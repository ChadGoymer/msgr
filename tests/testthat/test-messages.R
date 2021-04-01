# TEST: info -------------------------------------------------------------------

test_that("info displays a message, and records it in a log file", {

  log_dir <- fs::path_temp()
  log_path <- fs::path(log_dir, "test-info/msgr.log")

  expect_message(info("This is INFO", log_path = log_path), "This is INFO")
  expect_true(fs::file_exists(log_path))

  log_txt <- readLines(log_path)
  expect_match(log_txt[length(log_txt)], "INFO \\| This is INFO")

  expect_silent(
    info(
      "This is INFO",
      level     = 2,
      msg_types = "INFO",
      msg_level = 1,
      log_path  = log_path
    )
  )

  expect_silent(
    info(
      "This is INFO",
      level     = 1,
      msg_types = "ERROR",
      msg_level = 1,
      log_path  = log_path
    )
  )

  expect_message(
    info(
      "This is INFO",
      level     = 2,
      msg_types = "INFO",
      msg_level = 3,
      log_path  = log_path
    ),
    "This is INFO"
  )

  log_length <- length(readLines(log_path))

  expect_message(
    info(
      "This is INFO",
      level     = 1,
      msg_types = "INFO",
      msg_level = 1,
      log_path  = ""
    ),
    "This is INFO"
  )

  expect_identical(length(readLines(log_path)), log_length)

  expect_message(info("This ", "is ", "INFO"), "This is INFO")

})

test_that("invalid arguments for info throw an error", {

  expect_error(
    info("This is INFO", level = "a"),
    "'level' must be an integer between 1 and 10: a"
  )

  expect_error(
    info("This is INFO", level = 0),
    "'level' must be an integer between 1 and 10: 0"
  )

  expect_error(
    info("This is INFO", msg_types = 0),
    "'msg_types' must be NULL or a character vector: 0"
  )

  expect_error(
    info("This is INFO", msg_types = "BOB"),
    "'msg_types' must be either 'INFO', 'WARNING' or 'ERROR': BOB"
  )

  expect_error(
    info("This is INFO", msg_level = "a"),
    "'msg_level' must be an integer between 1 and 10: a"
  )

  expect_error(
    info("This is INFO", msg_level = -1),
    "'msg_level' must be an integer between 1 and 10: -1"
  )

  expect_error(
    info("This is INFO", log_path = 0),
    "'log_path' must be a string: 0"
  )

})

# TEST: warn -------------------------------------------------------------------

test_that("warn displays a warning, and records it in a log file", {

  log_dir <- fs::path_temp()
  log_path <- fs::path(log_dir, "test-warn/msgr.log")

  expect_warning(
    warn("This is a WARNING", log_path = log_path),
    "This is a WARNING"
  )
  expect_true(fs::file_exists(log_path))

  log_txt <- readLines(log_path)
  expect_match(log_txt[length(log_txt)], "WARNING \\| This is a WARNING")

  expect_silent(
    warn(
      "This is a WARNING",
      level     = 2,
      msg_types = "WARNING",
      msg_level = 1,
      log_path  = log_path
    )
  )

  expect_silent(
    warn(
      "This is a WARNING",
      level     = 1,
      msg_types = "ERROR",
      msg_level = 1,
      log_path  = log_path
    )
  )

  expect_warning(
    warn(
      "This is a WARNING",
      level     = 2,
      msg_types = "WARNING",
      msg_level = 3,
      log_path  = log_path
    ),
    "This is a WARNING"
  )

  log_length <- length(readLines(log_path))

  expect_warning(
    warn(
      "This is a WARNING",
      level     = 1,
      msg_types = "WARNING",
      msg_level = 1,
      log_path  = ""
    ),
    "This is a WARNING"
  )

  expect_identical(length(readLines(log_path)), log_length)

  expect_warning(warn("This ", "is ", "a ", "WARNING"), "This is a WARNING")

})

test_that("invalid arguments for warn throw an error", {

  expect_error(
    warn("This is a WARNING", level = "a"),
    "'level' must be an integer between 1 and 10: a"
  )

  expect_error(
    warn("This is a WARNING", level = 0),
    "'level' must be an integer between 1 and 10: 0"
  )

  expect_error(
    warn("This is a WARNING", msg_types = 0),
    "'msg_types' must be NULL or a character vector: 0"
  )

  expect_error(
    warn("This is a WARNING", msg_types = "BOB"),
    "'msg_types' must be either 'INFO', 'WARNING' or 'ERROR': BOB"
  )

  expect_error(
    warn("This is a WARNING", msg_level = "a"),
    "'msg_level' must be an integer between 1 and 10: a"
  )

  expect_error(
    warn("This is a WARNING", msg_level = -1),
    "'msg_level' must be an integer between 1 and 10: -1"
  )

  expect_error(
    warn("This is a WARNING", log_path = 0),
    "'log_path' must be a string: 0"
  )

})

# TEST: error ------------------------------------------------------------------

test_that("error displays a error, and records it in a log file", {

  log_dir <- fs::path_temp()
  log_path <- fs::path(log_dir, "test-error/msgr.log")

  expect_error(
    error("This is an ERROR", log_path = log_path),
    "This is an ERROR"
  )
  expect_true(fs::file_exists(log_path))

  log_txt <- readLines(log_path)
  expect_match(log_txt[length(log_txt)], "ERROR \\| This is an ERROR")

  expect_silent(
    error(
      "This is an ERROR",
      level     = 2,
      msg_types = "ERROR",
      msg_level = 1,
      log_path  = log_path
    )
  )

  expect_silent(
    error(
      "This is an ERROR",
      level     = 1,
      msg_types = "INFO",
      msg_level = 1,
      log_path  = log_path
    )
  )

  expect_error(
    error(
      "This is an ERROR",
      level     = 2,
      msg_types = "ERROR",
      msg_level = 3,
      log_path  = log_path
    ),
    "This is an ERROR"
  )

  log_length <- length(readLines(log_path))

  expect_error(
    error(
      "This is an ERROR",
      level     = 1,
      msg_types = "ERROR",
      msg_level = 1,
      log_path  = ""
    ),
    "This is an ERROR"
  )

  expect_identical(length(readLines(log_path)), log_length)

  expect_error(error("This ", "is ", "an ", "ERROR"), "This is an ERROR")

})

test_that("invalid arguments for warn throw an error", {

  expect_error(
    error("This is an ERROR", level = "a"),
    "'level' must be an integer between 1 and 10: a"
  )

  expect_error(
    error("This is an ERROR", level = 0),
    "'level' must be an integer between 1 and 10: 0"
  )

  expect_error(
    error("This is an ERROR", msg_types = 0),
    "'msg_types' must be NULL or a character vector: 0"
  )

  expect_error(
    error("This is an ERROR", msg_types = "BOB"),
    "'msg_types' must be either 'INFO', 'WARNING' or 'ERROR': BOB"
  )

  expect_error(
    error("This is an ERROR", msg_level = "a"),
    "'msg_level' must be an integer between 1 and 10: a"
  )

  expect_error(
    error("This is an ERROR", msg_level = -1),
    "'msg_level' must be an integer between 1 and 10: -1"
  )

  expect_error(
    error("This is an ERROR", log_path = 0),
    "'log_path' must be a string: 0"
  )

})

# TEST: info_if ----------------------------------------------------------------

test_that("info_if returns a message if the condition is true", {

  expect_message(info_if(2 > 1), "2 > 1 is true")

  test_info_if <- function(x, y) info_if(x > y)

  expect_message(test_info_if(2, 1), "In test_info_if\\(\\): x > y is true")
  expect_silent(test_info_if(1, 2))

  test_info_if_msg <- function(x, y) info_if(x > y, "This is rubbish")
  expect_message(
    test_info_if_msg(2, 1),
    "In test_info_if_msg\\(\\): This is rubbish"
  )

  test_info_if_split_msg <- function(x, y)
    info_if(x > y, "This ", "is ", "rubbish")

  expect_message(
    test_info_if_split_msg(2, 1),
    "In test_info_if_split_msg\\(\\): This is rubbish"
  )

})

# TEST: warn_if ----------------------------------------------------------------

test_that("warn_if returns a warning if the condition is true", {

  expect_warning(warn_if(2 > 1), "2 > 1 is true")

  test_warn_if <- function(x, y) warn_if(x > y)

  expect_warning(test_warn_if(2, 1), "In test_warn_if\\(\\): x > y is true")
  expect_silent(test_warn_if(1, 2))

  test_warn_if_msg <- function(x, y) warn_if(x > y, "This is rubbish")
  expect_warning(
    test_warn_if_msg(2, 1),
    "In test_warn_if_msg\\(\\): This is rubbish"
  )

  test_warn_if_split_msg <- function(x, y)
    warn_if(x > y, "This ", "is ", "rubbish")

  expect_warning(
    test_warn_if_split_msg(2, 1),
    "In test_warn_if_split_msg\\(\\): This is rubbish"
  )

})

# TEST: error_if ---------------------------------------------------------------

test_that("error_if returns an error if the condition is true", {

  expect_error(error_if(2 > 1), "2 > 1 is true")

  test_error_if <- function(x, y) error_if(x > y)

  expect_error(test_error_if(2, 1), "In test_error_if\\(\\): x > y is true")
  expect_silent(test_error_if(1, 2))

  test_error_if_msg <- function(x, y) error_if(x > y, "This is rubbish")
  expect_error(
    test_error_if_msg(2, 1),
    "In test_error_if_msg\\(\\): This is rubbish"
  )

  test_error_if_split_msg <- function(x, y)
    error_if(x > y, "This ", "is ", "rubbish")

  expect_error(
    test_error_if_split_msg(2, 1),
    "In test_error_if_split_msg\\(\\): This is rubbish"
  )

})
