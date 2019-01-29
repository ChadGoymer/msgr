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
