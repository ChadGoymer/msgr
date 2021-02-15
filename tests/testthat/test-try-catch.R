# TEST: try_catch --------------------------------------------------------------

test_that("try_catch catches messages, warnings and errors appropriately", {

  expect_error(try_catch(stop("This is an ERROR")), "This is an ERROR")

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

})
