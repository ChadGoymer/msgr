# TEST: try_catch --------------------------------------------------------------

test_that("try_catch catches messages, warnings and errors appropriately", {

  expect_error(try_catch(stop("This is an ERROR")), "This is an ERROR")

  error_message <- tryCatch(
    try_catch(stop("[00:00:00] This is an ERROR")),
    error = function(e) e$message
  )
  expect_match(error_message, "This is an ERROR")
  expect_false(grepl("\\[00:00:00\\]", error_message))

  expect_error(
    capture.output(
      try_catch(
        stop("This is an ERROR"),
        finally = print("done")
      )
    ),
    "This is an ERROR"
  )

  expect_output(
    try(
      try_catch(
        stop("This is an ERROR"),
        finally = print("done")
      ),
      silent = TRUE
    ),
    "done"
  )

})

# TEST: try_map ----------------------------------------------------------------

test_that("try_map catches errors and displays a warning", {

  test_try_map <- function(x, y) if (x > y) stop("x > y") else x

  expect_silent(
    try(
      try_map(1:3, test_try_map, 2, warn_level = 0),
      silent = TRUE
    )
  )
  expect_warning(
    try(
      try_map(1:3, test_try_map, 2, warn_level = 1),
      silent = TRUE
    ),
    "Failed for x = 3"
  )
  expect_error(
    suppressWarnings(try_map(1:3, test_try_map, 2)),
    "In test_try_map\\(\\): x > y"
  )

  test_try_char <- function(x) if (nchar(x) > 20) stop("string too long") else x

  expect_warning(
    try(
      try_map(paste(LETTERS, collapse = ""), test_try_char, warn_level = 1),
      silent = TRUE
    ),
    "Failed for x = ABCDEFGHIJKLMNOPQRST..."
  )

  expect_silent(try_map(1:3, test_try_map, 5))
  ok_result <- try_map(1:3, test_try_map, 5)

  expect_is(ok_result, "list")
  expect_identical(ok_result, as.list(1:3))

  expect_silent(
    try(
      try_map(
        x = 1:3,
        f = function(x, y) if (x > y) stop("x > y") else x,
        2,
        warn_level = 0
      ),
      silent = TRUE
    )
  )

  expect_warning(
    try(
      try_map(
        x = 1:3,
        f = function(x, y) if (x > y) stop("x > y") else x,
        2,
        warn_level = 1
      ),
      silent = TRUE
    ),
    "Failed for x = 3"
  )

  expect_error(
    suppressWarnings(
      try_map(1:3, function(x, y) if (x > y) stop("x > y") else x, 2)
    ),
    "x > y"
  )

  expect_error(
    suppressWarnings(
      try_map(3, function(x, y) if (x > y) stop("x > y") else x, 2)
    ),
    "x > y"
  )

  expect_warning(
    try_map(1:3, test_try_map, 2, on_error = "warn", warn_level = 0),
    "In test_try_map\\(\\): x > y"
  )

  expect_message(
    try_map(1:3, test_try_map, 2, on_error = "info", warn_level = 0),
    "In test_try_map\\(\\): x > y"
  )

  expect_identical(
    try_map(c("bob", "jane"), nchar),
    list(bob = 3L, jane = 4L)
  )

  expect_identical(
    try_map(c("bob", "jane"), nchar, use_names = FALSE),
    list(3L, 4L)
  )

  expect_error(
    try_map(c("bob", "jane"), stop),
    "'bob': In stop\\(\\): bob\n'jane': In stop\\(\\): jane"
  )

  expect_identical(
    try_map(c("bob", "jane"), nchar, simplify = TRUE),
    c(bob = 3L, jane = 4L)
  )

})

# TEST: try_pmap ---------------------------------------------------------------

test_that("try_pmap catches errors and displays a warning", {

  test_try_pmap <- function(x, y) if (x > y) stop("x > y") else x

  expect_silent(
    try(
      try_pmap(list(1:3, 3:1), test_try_pmap, warn_level = 0),
      silent = TRUE
    )
  )

  expect_warning(
    try(
      try_pmap(list(1:3, 3:1), test_try_pmap, warn_level = 1),
      silent = TRUE
    ),
    "Failed for x = 3"
  )

  expect_error(
    suppressWarnings(
      try_pmap(list(1:3, 3:1), test_try_pmap)
    ),
    "In test_try_pmap\\(\\): x > y"
  )

  test_try_char <- function(x) if (nchar(x) > 20) stop("string too long") else x

  expect_warning(
    try(
      try_pmap(
        list(paste(LETTERS, collapse = "")),
        test_try_char,
        warn_level = 1
      ),
      silent = TRUE
    ),
    "Failed for x = ABCDEFGHIJKLMNOPQRST..."
  )

  expect_silent(try_pmap(list(1:3, 2:4), test_try_pmap))
  ok_result <- try_pmap(list(1:3, 2:4), test_try_pmap)

  expect_is(ok_result, "list")
  expect_identical(ok_result, as.list(1:3))

  expect_silent(
    try(
      try_pmap(
        l = list(1:3, 3:1),
        f = function(x, y) if (x > y) stop("x > y") else x,
        warn_level = 0
      ),
      silent = TRUE
    )
  )

  expect_warning(
    try(
      try_pmap(
        l = list(1:3, 3:1),
        f = function(x, y) if (x > y) stop("x > y") else x,
        warn_level = 1
      ),
      silent = TRUE
    ),
    "Failed for x = 3"
  )

  expect_error(
    suppressWarnings(
      try_pmap(list(1:3, 3:1), function(x, y) if (x > y) stop("x > y") else x)
    ),
    "x > y"
  )

  expect_error(
    suppressWarnings(
      try_pmap(list(3, 1), function(x, y) if (x > y) stop("x > y") else x)
    ),
    "x > y"
  )

  expect_warning(
    try_pmap(list(1:3, 3:1), test_try_pmap, on_error = "warn", warn_level = 0),
    "In test_try_pmap\\(\\): x > y"
  )

  expect_message(
    try_pmap(list(1:3, 3:1), test_try_pmap, on_error = "info", warn_level = 0),
    "In test_try_pmap\\(\\): x > y"
  )

  expect_identical(
    try_pmap(
      list(c("bob", "jim"), c("jane", "brenda")),
      function(x, y) nchar(paste0(x, y))
    ),
    list(bob = 7L, jim = 9L)
  )

  expect_identical(
    try_pmap(
      list(c("bob", "jim"), c("jane", "brenda")),
      function(x, y) nchar(paste0(x, y)),
      use_names = FALSE
    ),
    list(7L, 9L)
  )

  expect_error(
    try_pmap(
      list(c("bob", "jim"), c("jane", "brenda")),
      function(x, y) stop(paste(x, y))
    ),
    "'bob': bob jane\n'jim': jim brenda"
  )

  expect_identical(
    try_pmap(
      list(c("bob", "jim"), c("jane", "brenda")),
      function(x, y) nchar(paste0(x, y)),
      simplify = TRUE
    ),
    c(bob = 7L, jim = 9L)
  )

})

# TEST: remove_time ------------------------------------------------------------

test_that("remove_time removes time component if it is there", {

  expect_identical(
    remove_time("This is a message without time"),
    "This is a message without time"
  )
  expect_identical(
    remove_time("[12:54:12] This is a message with time"),
    "This is a message with time"
  )

})
