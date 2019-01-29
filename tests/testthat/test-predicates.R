context("predicates")

# TEST: is_vector -----------------------------------------------------------------------------

test_that("is_vector returns TRUE for valid vectors", {

  expect_true(is_vector(TRUE))
  expect_true(is_vector(1L))
  expect_true(is_vector(3.142))
  expect_true(is_vector("bob"))

  expect_true(is_vector(c(TRUE, FALSE)))
  expect_true(is_vector(1:3))
  expect_true(is_vector(c(3.142, 2.718, 1.414)))
  expect_true(is_vector(c("bob", "jane", "jim")))

  expect_true(is_vector(NA))

})

test_that("is_vector returns FALSE for invalid vectors", {

  expect_false(is_vector(list(1:3)))
  expect_false(is_vector(matrix(1:4, nrow = 2)))
  expect_false(is_vector(data.frame(x = LETTERS[1:3], y = 1:3)))
  expect_false(is_vector(NULL))

})

# TEST: is_scalar -----------------------------------------------------------------------------

test_that("is_scalar returns TRUE for valid vectors", {

  expect_true(is_scalar(TRUE))
  expect_true(is_scalar(1L))
  expect_true(is_scalar(3.142))
  expect_true(is_scalar("bob"))

})

test_that("is_scalar returns FALSE for invalid vectors", {

  expect_false(is_scalar(c(TRUE, FALSE)))
  expect_false(is_scalar(1:3))
  expect_false(is_scalar(c(3.142, 2.718, 1.414)))
  expect_false(is_scalar(c("bob", "jane", "jim")))

  expect_false(is_scalar(list(1)))
  expect_false(is_scalar(matrix(1, nrow = 1)))
  expect_false(is_scalar(data.frame(x = "A")))
  expect_false(is_scalar(NULL))

})

# TEST: is_logical ----------------------------------------------------------------------------

test_that("is_logical returns TRUE for valid vectors", {

  expect_true(is_logical(TRUE))
  expect_true(is_logical(c(TRUE, FALSE)))
  expect_true(is_logical(NA))

})

test_that("is_logical returns FALSE for invalid vectors", {

  expect_false(is_logical(1:3))
  expect_false(is_logical(c(3.142, 2.718, 1.414)))
  expect_false(is_logical(c("bob", "jane", "jim")))

  expect_false(is_logical(list(1:3)))
  expect_false(is_logical(matrix(1:4, nrow = 2)))
  expect_false(is_logical(data.frame(x = LETTERS[1:3], y = 1:3)))
  expect_false(is_logical(NULL))

})

# TEST: is_boolean ----------------------------------------------------------------------------

test_that("is_boolean returns TRUE for valid vectors", {

  expect_true(is_boolean(TRUE))
  expect_true(is_boolean(NA))

})

test_that("is_boolean returns FALSE for invalid vectors", {

  expect_false(is_boolean(c(TRUE, FALSE)))
  expect_false(is_boolean(1:3))
  expect_false(is_boolean(c(3.142, 2.718, 1.414)))
  expect_false(is_boolean(c("bob", "jane", "jim")))

  expect_false(is_boolean(list(1:3)))
  expect_false(is_boolean(matrix(1:4, nrow = 2)))
  expect_false(is_boolean(data.frame(x = LETTERS[1:3], y = 1:3)))
  expect_false(is_boolean(NULL))

})

# TEST: is_numeric ----------------------------------------------------------------------------

test_that("is_numeric returns TRUE for valid vectors", {

  expect_true(is_numeric(1L))
  expect_true(is_numeric(3.142))
  expect_true(is_numeric(1:3))
  expect_true(is_numeric(c(3.142, 2.718, 1.414)))
  expect_true(is_numeric(NA_real_))

})

test_that("is_numeric returns FALSE for invalid vectors", {

  expect_false(is_numeric(c(TRUE, FALSE)))
  expect_false(is_numeric(c("bob", "jane", "jim")))

  expect_false(is_numeric(list(1:3)))
  expect_false(is_numeric(matrix(1:4, nrow = 2)))
  expect_false(is_numeric(data.frame(x = LETTERS[1:3], y = 1:3)))
  expect_false(is_numeric(NULL))

})

# TEST: is_number ----------------------------------------------------------------------------

test_that("is_number returns TRUE for valid vectors", {

  expect_true(is_number(1L))
  expect_true(is_number(3.142))
  expect_true(is_number(NA_real_))

})

test_that("is_number returns FALSE for invalid vectors", {

  expect_false(is_number(1:3))
  expect_false(is_number(c(3.142, 2.718, 1.414)))
  expect_false(is_number(c(TRUE, FALSE)))
  expect_false(is_number(c("bob", "jane", "jim")))

  expect_false(is_number(list(1:3)))
  expect_false(is_number(matrix(1:4, nrow = 2)))
  expect_false(is_number(data.frame(x = LETTERS[1:3], y = 1:3)))
  expect_false(is_number(NULL))

})
