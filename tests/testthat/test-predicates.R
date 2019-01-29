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
