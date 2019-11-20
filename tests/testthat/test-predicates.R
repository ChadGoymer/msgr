context("predicates")


# TEST: is_na ---------------------------------------------------------------------------------

test_that("is_na returns TRUE for NA", {

  expect_true(is_na(NA))
  expect_true(is_na(NA_integer_))
  expect_true(is_na(NA_real_))
  expect_true(is_na(NA_complex_))
  expect_true(is_na(NA_character_))

  expect_identical(is_na(c(1, NA, 3)), c(FALSE, TRUE, FALSE))
  expect_identical(
    is_na(matrix(c(1, 2, NA, 4), nrow = 2)),
    matrix(c(FALSE, FALSE, TRUE, FALSE), nrow = 2))
  expect_identical(
    is_na(data.frame(x = LETTERS[1:3], y = c(1, NA, 3))),
    matrix(c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE), nrow = 3, dimnames = list(NULL, c("x", "y"))))

})

test_that("is_na returns FALSE for non-NAs", {

  expect_false(is_na(TRUE))
  expect_false(is_na(1L))
  expect_false(is_na(3.142))
  expect_false(is_na("bob"))

  expect_false(is_na(NULL))
  expect_false(is_na(list(1:3)))

})


# TEST: is_url --------------------------------------------------------------------------------

test_that("is_url returns TRUE for valid URLs", {

  expect_true(is_url("https://www.secure.com"))
  expect_true(is_url("http://www.insecure.com"))

  expect_true(is_url("https://www.secure.com/some/endpoint"))
  expect_true(is_url("http://www.insecure.com/some/endpoint"))

})

test_that("is_url returns FALSE for invalid URLs", {

  expect_false(is_url(TRUE))
  expect_false(is_url(3.142))
  expect_false(is_url("bob"))
  expect_false(is_url(list(1:3)))
  expect_false(is_url(matrix(1:4, nrow = 2)))
  expect_false(is_url(data.frame(x = LETTERS[1:3], y = 1:3)))

  expect_false(is_url(c("https://www.secure.com/endpoint1", "https://www.secure.com/endpoint2")))

})

# TEST: is_dir --------------------------------------------------------------------------------

test_that("is_dir returns TRUE for an existing directory", {

  temp_dir <- tempdir()

  expect_true(is_dir(temp_dir))

})

test_that("is_dir returns FALSE for an invalid directory", {

  temp_dir <- tempdir()
  no_dir <- file.path(temp_dir, "does_not_exist")

  expect_false(is_dir("C:/does/not/exist"))
  expect_false(is_dir(no_dir))
  expect_false(is_dir(c(temp_dir, temp_dir)))

})

# TEST: is_file -------------------------------------------------------------------------------

test_that("is_file returns TRUE for an existing directory", {

  temp_file <- tempfile(fileext = ".txt")
  file.create(temp_file)

  expect_true(is_file(temp_file))

})

test_that("is_file returns FALSE for an invalid directory", {

  temp_file <- tempfile(fileext = ".txt")
  file.create(temp_file)
  no_file <- file.path(tempdir(), "does_not_exist.txt")

  expect_false(is_file("C:/does/not/exist.txt"))
  expect_false(is_file(no_file))
  expect_false(is_file(c(temp_file, temp_file)))

})

# TEST: is_readable ---------------------------------------------------------------------------

test_that("is_readable returns TRUE for a readable file or directory", {

  temp_dir <- tempdir()

  temp_file <- file.path(temp_dir, "readable_file.txt")
  file.create(temp_file)

  expect_true(is_readable(temp_dir))
  expect_true(is_readable(temp_file))

})

test_that("is_readable returns FALSE for an unreadable file or directory", {

  temp_dir <- tempdir()
  no_dir <- file.path(temp_dir, "does_not_exist")

  temp_file <- file.path(temp_dir, "unreadable_file.txt")

  expect_false(is_readable("C:/does/not/exist"))
  expect_false(is_readable(no_dir))
  expect_false(is_readable(temp_file))

})

# TEST: is_writeable --------------------------------------------------------------------------

test_that("is_writeable returns TRUE for a readable file or directory", {

  temp_dir <- tempdir()

  temp_file <- file.path(temp_dir, "writeable_file.txt")
  file.create(temp_file)

  expect_true(is_writeable(temp_dir))
  expect_true(is_writeable(temp_file))

})

test_that("is_writeable returns FALSE for an unreadable file or directory", {

  temp_dir <- tempdir()
  no_dir <- file.path(temp_dir, "does_not_exist")

  temp_file <- file.path(temp_dir, "unwriteable_file.txt")

  expect_false(is_writeable("C:/does/not/exist"))
  expect_false(is_writeable(no_dir))
  expect_false(is_writeable(temp_file))

})

# TEST: is_in ---------------------------------------------------------------------------------

test_that("is_in returns TRUE if all elements of the first variable are in the second", {

  expect_true(is_in(1, 1:3))
  expect_true(is_in(c("a", "b"), letters))
  expect_true(is_in(list("a", "b"), as.list(letters)))

})

test_that("is_in returns FALSE if not all the elements of the first variable are in the second", {

  expect_false(is_in(0, 1:3))
  expect_false(is_in(LETTERS, c("A", "B", "C")))

})

# TEST: has_names -----------------------------------------------------------------------------

test_that("has_names returns TRUE for an object with names", {

  expect_true(has_names(c(a = 1, b = 2)))
  expect_true(has_names(list(a = 1, b = 2)))
  expect_true(has_names(data.frame(a = 1, b = 2)))

  expect_true(has_names(c(a = 1, b = 2), "a"))
  expect_true(has_names(list(a = 1, b = 2), c("a", "b")))
  expect_true(has_names(data.frame(a = 1, b = 2), "a"))

})

test_that("has_names returns FALSE for an object without names", {

  expect_false(has_names(1:3))
  expect_false(has_names("bob"))
  expect_false(has_names(list(1:3)))

  expect_false(has_names(c(a = 1, b = 2), "c"))
  expect_false(has_names(list(a = 1, b = 2), c("b", "c")))
  expect_false(has_names(data.frame(a = 1, b = 2), "c"))

  expect_false(has_names(1:3, "a"))
  expect_false(has_names("bob", "a"))
  expect_false(has_names(list(1:3), "a"))

})
