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

# TEST: is_integer ----------------------------------------------------------------------------

test_that("is_integer returns TRUE for valid vectors", {

  expect_true(is_integer(1L))
  expect_true(is_integer(1:3))
  expect_true(is_integer(NA_integer_))

})

test_that("is_integer returns FALSE for invalid vectors", {

  expect_false(is_integer(3.142))
  expect_false(is_integer(c(3.142, 2.718, 1.414)))
  expect_false(is_integer(c(TRUE, FALSE)))
  expect_false(is_integer(c("bob", "jane", "jim")))

  expect_false(is_integer(list(1:3)))
  expect_false(is_integer(matrix(1:4, nrow = 2)))
  expect_false(is_integer(data.frame(x = LETTERS[1:3], y = 1:3)))
  expect_false(is_integer(NULL))

})

# TEST: is_natural ----------------------------------------------------------------------------

test_that("is_natural returns TRUE for valid vectors", {

  expect_true(is_natural(1))

})

test_that("is_natural returns FALSE for invalid vectors", {

  expect_false(is_natural(-1))
  expect_false(is_natural(1:3))
  expect_false(is_natural(NA_integer_))
  expect_false(is_natural(3.142))
  expect_false(is_natural(c(3.142, 2.718, 1.414)))
  expect_false(is_natural(c(TRUE, FALSE)))
  expect_false(is_natural(c("bob", "jane", "jim")))

  expect_false(is_natural(list(1:3)))
  expect_false(is_natural(matrix(1:4, nrow = 2)))
  expect_false(is_natural(data.frame(x = LETTERS[1:3], y = 1:3)))
  expect_false(is_natural(NULL))

})

# TEST: is_character --------------------------------------------------------------------------

test_that("is_character returns TRUE for valid vectors", {

  expect_true(is_character("bob"))
  expect_true(is_character(c("bob", "jane", "jim")))
  expect_true(is_character(NA_character_))

})

test_that("is_character returns FALSE for invalid vectors", {

  expect_false(is_character(TRUE))
  expect_false(is_character(1L))
  expect_false(is_character(3.142))

  expect_false(is_character(c(TRUE, FALSE)))
  expect_false(is_character(1:3))
  expect_false(is_character(c(3.142, 2.718, 1.414)))

  expect_false(is_character(list(1:3)))
  expect_false(is_character(matrix(1:4, nrow = 2)))
  expect_false(is_character(data.frame(x = LETTERS[1:3], y = 1:3)))
  expect_false(is_character(NULL))

})

# TEST: is_string -----------------------------------------------------------------------------

test_that("is_string returns TRUE for valid vectors", {

  expect_true(is_string("bob"))
  expect_true(is_string(NA_character_))

})

test_that("is_string returns FALSE for invalid vectors", {

  expect_false(is_string(c("bob", "jane", "jim")))

  expect_false(is_string(TRUE))
  expect_false(is_string(1L))
  expect_false(is_string(3.142))

  expect_false(is_string(c(TRUE, FALSE)))
  expect_false(is_string(1:3))
  expect_false(is_string(c(3.142, 2.718, 1.414)))

  expect_false(is_string(list(1:3)))
  expect_false(is_string(matrix(1:4, nrow = 2)))
  expect_false(is_string(data.frame(x = LETTERS[1:3], y = 1:3)))
  expect_false(is_string(NULL))

})

# TEST: is_list -------------------------------------------------------------------------------

test_that("is_list returns TRUE for valid lists", {

  expect_true(is_list(list(1:3)))
  expect_true(is_list(data.frame(x = LETTERS[1:3], y = 1:3)))

})

test_that("is_list returns FALSE for invalid lists", {

  expect_false(is_list(TRUE))
  expect_false(is_list(1L))
  expect_false(is_list(3.142))
  expect_false(is_list("bob"))

  expect_false(is_list(c(TRUE, FALSE)))
  expect_false(is_list(1:3))
  expect_false(is_list(c(3.142, 2.718, 1.414)))
  expect_false(is_list(c("bob", "jane", "jim")))

  expect_false(is_list(matrix(1:4, nrow = 2)))
  expect_false(is_list(NA))
  expect_false(is_list(NULL))

})

# TEST: is_data_frame -------------------------------------------------------------------------

test_that("is_data_frame returns TRUE for valid data.frames", {

  expect_true(is_data_frame(data.frame(x = LETTERS[1:3], y = 1:3)))

})

test_that("is_data_frame returns FALSE for invalid data.frames", {

  expect_false(is_data_frame(TRUE))
  expect_false(is_data_frame(1L))
  expect_false(is_data_frame(3.142))
  expect_false(is_data_frame("bob"))

  expect_false(is_data_frame(c(TRUE, FALSE)))
  expect_false(is_data_frame(1:3))
  expect_false(is_data_frame(c(3.142, 2.718, 1.414)))
  expect_false(is_data_frame(c("bob", "jane", "jim")))

  expect_false(is_data_frame(list(1:3)))
  expect_false(is_data_frame(matrix(1:4, nrow = 2)))
  expect_false(is_data_frame(NA))
  expect_false(is_data_frame(NULL))

})

# TEST: is_function ---------------------------------------------------------------------------

test_that("is_function returns TRUE for valid functions", {

  expect_true(is_function(mean))
  expect_true(is_function(function(x) x))

  identity_fn <- function(x) x
  expect_true(is_function(identity_fn))

})

test_that("is_function returns FALSE for invalid functions", {

  expect_false(is_function(TRUE))
  expect_false(is_function(1L))
  expect_false(is_function(3.142))
  expect_false(is_function("bob"))

})

# TEST: is_null -------------------------------------------------------------------------------

test_that("is_null returns TRUE for NULL", {

  expect_true(is_null(NULL))

})

test_that("is_null returns FALSE for non-NULLs", {

  expect_false(is_null(TRUE))
  expect_false(is_null(1L))
  expect_false(is_null(3.142))
  expect_false(is_null("bob"))

  expect_false(is_null(NA))
  expect_false(is_null(list(1:3)))
  expect_false(is_null(matrix(1:4, nrow = 2)))
  expect_false(is_null(data.frame(x = LETTERS[1:3], y = 1:3)))

})

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
