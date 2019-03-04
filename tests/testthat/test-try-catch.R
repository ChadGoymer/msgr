context("try catch")

# TEST: try_catch -----------------------------------------------------------------------------

test_that("try_catch catches messages, warnings and errors appropriately", {

  expect_error(try_catch(stop("This is an ERROR")), "This is an ERROR")

  expect_error(
    capture.output(try_catch(stop("This is an ERROR"), finally = print("done"))),
    "This is an ERROR")

  expect_output(
    try(try_catch(stop("This is an ERROR"), finally = print("done")), silent = TRUE),
    "done")

})
