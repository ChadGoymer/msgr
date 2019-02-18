context("messages")

# TEST: info ----------------------------------------------------------------------------------

test_that("info displays a message, and records it in a log file", {

  log_dir <- tempdir()
  log_path <- file.path(log_dir, "test-info.log")

  expect_message(info("This is INFO", log_path = log_path), "This is INFO")
  expect_true(file.exists(log_path))

  log_txt <- readLines(log_path)
  expect_match(log_txt[length(log_txt)], "INFO \\| This is INFO")

  expect_silent(
    info("This is INFO", level = 2, msg_types = "INFO", msg_level = 1, log_path = log_path))

  expect_silent(
    info("This is INFO", level = 1, msg_types = "ERROR", msg_level = 1, log_path = log_path))

  expect_message(
    info("This is INFO", level = 2, msg_types = "INFO", msg_level = 3, log_path = log_path),
    "This is INFO")

  log_length <- length(readLines(log_path))

  expect_message(
    info("This is INFO", level = 1, msg_types = "INFO", msg_level = 1, log_path = ""),
    "This is INFO")

  expect_identical(length(readLines(log_path)), log_length)

})

test_that("invalid arguments for info throw an error", {

  expect_error(
    info("This is INFO", level = "a"),
    "'level' must be an integer between 1 and 10: a")

  expect_error(
    info("This is INFO", level = 0),
    "'level' must be an integer between 1 and 10: 0")

  expect_error(
    info("This is INFO", msg_types = 0),
    "'msg_types' must be NULL \\(no messages\\) or a character vector containing \"INFO\", \"WARNING\" or \"ERROR\": 0")

  expect_error(
    info("This is INFO", msg_types = "BOB"),
    "'msg_types' must be NULL \\(no messages\\) or a character vector containing \"INFO\", \"WARNING\" or \"ERROR\": BOB")

  expect_error(
    info("This is INFO", msg_level = "a"),
    "'msg_level' must be an integer between 1 and 10: a")

  expect_error(
    info("This is INFO", msg_level = 0),
    "'msg_level' must be an integer between 1 and 10: 0")

  expect_error(
    info("This is INFO", log_path = 0),
    "'log_path' must be a string: 0")

})
