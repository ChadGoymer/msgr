test_that("evironment variables are set correctly on load", {

  expect_identical(Sys.getenv("MSGR_LEVEL"), "1")
  expect_identical(Sys.getenv("MSGR_TYPES"), "INFO|WARNING|ERROR")
  expect_identical(Sys.getenv("MSGR_LOG_PATH"), "")

})

test_that("package options are set correctly on load", {

  expect_identical(getOption("msgr.level"), 1L)
  expect_identical(getOption("msgr.types"), c("INFO", "WARNING", "ERROR"))
  expect_identical(getOption("msgr.log_path"), "")

})
