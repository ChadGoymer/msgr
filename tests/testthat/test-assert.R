# TEST: assert -----------------------------------------------------------------

test_that("assert returns an error if the condition is false", {

  expect_error(assert(2 < 1), "2 < 1 is false")
  expect_silent(assert(2 > 1))

  test_assert <- function(x, y) assert(x > y)

  expect_error(test_assert(1, 2), "In test_assert\\(\\): x > y is false")
  expect_silent(test_assert(2, 1))

  test_assert_msg <- function(x, y) assert(x > y, "This is rubbish")
  expect_error(
    test_assert_msg(1, 2),
    "In test_assert_msg\\(\\): This is rubbish"
  )

  test_assert_split_msg <- function(x, y)
    assert(x > y, "This ", "is ", "rubbish")

  expect_error(
    test_assert_split_msg(1, 2),
    "In test_assert_split_msg\\(\\): This is rubbish"
  )

})
