# TEST: is_type ----------------------------------------------------------------

test_that("is_type returns TRUE for valid inputs", {

  expect_true(is_type(TRUE, "logical"))
  expect_true(is_type(1:3, "integer"))
  expect_true(is_type(c(1.414, 3.142), "double"))
  expect_true(is_type(letters, "character"))
  expect_true(is_type(matrix(1:4, nrow = 2), "integer"))
  expect_true(is_type(list(a = 1:3), "list"))
  expect_true(is_type(data.frame(a = 1:3), "list"))

  expect_true(is_type(TRUE, "logical", n = 1))
  expect_true(is_type(1:3, "integer", n = 3))
  expect_true(is_type(c(1.414, 3.142), "double", n = 2))
  expect_true(is_type(letters, "character", n = 26))
  expect_true(is_type(matrix(1:4, nrow = 2), "integer", n = 4))
  expect_true(is_type(list(a = 1:3), "list", n = 1))
  expect_true(is_type(data.frame(a = 1:3), "list", n = 1))

})

test_that("is_type returns FALSE for invalid inputs", {

  expect_false(is_type(TRUE, "boolean"))
  expect_false(is_type(1:3, "character"))
  expect_false(is_type(c(1.414, 3.142), "integer"))
  expect_false(is_type(letters, "list"))
  expect_false(is_type(matrix(1:4, nrow = 2), "double"))
  expect_false(is_type(list(a = 1:3), "vector"))
  expect_false(is_type(data.frame(a = 1:3), "data.frame"))

  expect_false(is_type(TRUE, "logical", n = 10))
  expect_false(is_type(1:3, "integer", n = 1))
  expect_false(is_type(c(1.414, 3.142), "double", n = 1))
  expect_false(is_type(letters, "character", n = 1))
  expect_false(is_type(matrix(1:4, nrow = 2), "integer", n = 2))
  expect_false(is_type(list(a = 1:3), "list", n = 3))
  expect_false(is_type(data.frame(a = 1:3), "list", n = 3))

})

# TEST: is_class ---------------------------------------------------------------

test_that("is_class returns TRUE for valid inputs", {

  expect_true(is_class(TRUE, "logical"))
  expect_true(is_class(1:3, "integer"))
  expect_true(is_class(c(1.414, 3.142), "numeric"))
  expect_true(is_class(letters, "character"))
  expect_true(is_class(matrix(1:4, nrow = 2), "matrix"))
  expect_true(is_class(list(a = 1:3), "list"))
  expect_true(is_class(data.frame(a = 1:3), "data.frame"))

  expect_true(is_class(TRUE, "logical", n = 1))
  expect_true(is_class(1:3, "integer", n = 3))
  expect_true(is_class(c(1.414, 3.142), "numeric", n = 2))
  expect_true(is_class(letters, "character", n = 26))
  expect_true(is_class(matrix(1:4, nrow = 2), "matrix", n = 4))
  expect_true(is_class(list(a = 1:3), "list", n = 1))
  expect_true(is_class(data.frame(a = 1:3), "data.frame", n = 1))

})

test_that("is_class returns FALSE for invalid inputs", {

  expect_false(is_class(TRUE, "boolean"))
  expect_false(is_class(1:3, "character"))
  expect_false(is_class(c(1.414, 3.142), "double"))
  expect_false(is_class(letters, "list"))
  expect_false(is_class(matrix(1:4, nrow = 2), "integer"))
  expect_false(is_class(list(a = 1:3), "vector"))
  expect_false(is_class(data.frame(a = 1:3), "list"))

  expect_false(is_class(TRUE, "logical", n = 10))
  expect_false(is_class(1:3, "integer", n = 1))
  expect_false(is_class(c(1.414, 3.142), "numeric", n = 1))
  expect_false(is_class(letters, "character", n = 1))
  expect_false(is_class(matrix(1:4, nrow = 2), "matrix", n = 2))
  expect_false(is_class(list(a = 1:3), "list", n = 3))
  expect_false(is_class(data.frame(a = 1:3), "data.frame", n = 3))

})

#' # TRUE
#' is_matrix(matrix(1:3))
#' is_matrix(matrix(1:12, nrow = 3, ncol = 4), n_col = 4)
#' is_matrix(matrix(1:12, nrow = 3, ncol = 4), n_row = 3)
#' # FALSE
#' is_matrix(1:3)
#' is_matrix(matrix(1:12, nrow = 3, ncol = 4), n_col = 3)
#' is_matrix(matrix(1:12, nrow = 3, ncol = 4), n_row = 4)
#'
#' # TRUE
#' is_data_frame(data.frame(a = 1:26, b = letters))
#' is_data_frame(data.frame(a = 1:26, b = letters), n_col = 2)
#' is_data_frame(data.frame(a = 1:26, b = letters), n_row = 26)
#' # FALSE
#' is_data_frame(list(a = 1:26, b = letters))
#' is_data_frame(data.frame(a = 1:26, b = letters), n_col = 5)
#' is_data_frame(data.frame(a = 1:26, b = letters), n_row = 5)


# TEST: is_natural -------------------------------------------------------------

test_that("is_natural returns TRUE for valid inputs", {

  expect_true(is_natural(1L))
  expect_true(is_natural(2.0))
  expect_true(is_natural(1:3))
  expect_true(is_natural(c(1.0, 2.0, 3.0)))
  expect_true(is_natural(matrix(1:4, nrow = 2)))

  expect_true(is_natural(1L, n = 1))
  expect_true(is_natural(2.0, n = 1))

})

test_that("is_natural returns FALSE for invalid inputs", {

  expect_false(is_natural(TRUE))
  expect_false(is_natural(3.142))
  expect_false(is_natural("bob"))
  expect_false(is_natural(list(1:3)))
  expect_false(is_natural(data.frame(x = LETTERS, y = 1:26)))

  expect_false(is_natural(1:3, n = 1))
  expect_false(is_natural(c(1.0, 2.0, 3.0), n = 1))
  expect_false(is_natural(matrix(1:4, nrow = 2), n = 1))

})

# TEST: is_number -------------------------------------------------------------

test_that("is_number returns TRUE for valid inputs", {

  expect_true(is_number(1L))
  expect_true(is_number(2.5))
  expect_true(is_number(1:3))
  expect_true(is_number(c(1.1, 2.2, 3.3)))
  expect_true(is_number(matrix(1:4, nrow = 2)))

  expect_true(is_number(1L, n = 1))
  expect_true(is_number(2.2, n = 1))

})

test_that("is_number returns FALSE for invalid inputs", {

  expect_false(is_number(TRUE))
  expect_false(is_number("bob"))
  expect_false(is_number(list(1:3)))
  expect_false(is_number(data.frame(x = LETTERS, y = 1:26)))

  expect_false(is_number(1:3, n = 1))
  expect_false(is_number(c(1.1, 2.2, 3.3), n = 1))
  expect_false(is_number(matrix(1:4, nrow = 2), n = 1))

})

# TEST: is_factor --------------------------------------------------------------

test_that("is_factor returns TRUE for valid inputs", {

  expect_true(is_factor(factor(1, levels = 1)))
  expect_true(is_factor(factor(c("a", "b", "a"))))

  expect_true(is_factor(factor(1, levels = 1), levels = "1"))
  expect_true(is_factor(factor(c("a", "b", "a")), levels = c("a", "b")))

  expect_true(is_factor(factor(1, levels = 1), n = 1))
  expect_true(is_factor(factor(c("a", "b", "a")), n = 3))

})

test_that("is_factor returns FALSE for invalid inputs", {

  expect_false(is_factor(TRUE))
  expect_false(is_factor(1:3))
  expect_false(is_factor(c(1.1, 2.2, 3.3)))
  expect_false(is_factor(matrix(1:4, nrow = 2)))
  expect_false(is_factor("bob"))
  expect_false(is_factor(list(1:3)))
  expect_false(is_factor(data.frame(x = LETTERS, y = 1:26)))

  expect_false(is_factor(factor(1, levels = 1), levels = "2"))
  expect_false(is_factor(factor(c("a", "b", "a")), levels = c("A", "B")))

  expect_false(is_factor(factor(1, levels = 1), n = 3))
  expect_false(is_factor(factor(c("a", "b", "a")), n = 1))

})

# TEST: is_array ---------------------------------------------------------------

test_that("is_array returns TRUE for valid inputs", {

  expect_true(is_array(array(1:3)))
  expect_true(is_array(array(1:12, dim = c(3, 4))))
  expect_true(is_array(matrix(1:4, nrow = 2)))

  expect_true(is_array(array(1:3), dims = 3))
  expect_true(is_array(array(1:12, dim = c(3, 4)), dims = c(3, 4)))
  expect_true(is_array(matrix(1:4, nrow = 2), dims = c(2, 2)))

})

test_that("is_array returns FALSE for invalid inputs", {

  expect_false(is_array(TRUE))
  expect_false(is_array(1:3))
  expect_false(is_array(c(1.1, 2.2, 3.3)))
  expect_false(is_array("bob"))
  expect_false(is_array(list(1:3)))
  expect_false(is_array(data.frame(x = LETTERS, y = 1:26)))

  expect_false(is_array(array(1:3), dims = 1))
  expect_false(is_array(array(1:12, dim = c(3, 4)), dims = c(4, 3)))
  expect_false(is_array(matrix(1:4, nrow = 2), dims = c(4, 4)))

})

# TEST: is_matrix ---------------------------------------------------------------

test_that("is_matrix returns TRUE for valid inputs", {

  expect_true(is_matrix(matrix(1:3)))
  expect_true(is_matrix(matrix(1:4, nrow = 2)))

  expect_true(is_matrix(matrix(1:3), n_col = 1))
  expect_true(is_matrix(matrix(1:12, ncol = 3), n_row = 4))

})

test_that("is_matrix returns FALSE for invalid inputs", {

  expect_false(is_matrix(TRUE))
  expect_false(is_matrix(1:3))
  expect_false(is_matrix(c(1.1, 2.2, 3.3)))
  expect_false(is_matrix("bob"))
  expect_false(is_matrix(array(1:3)))
  expect_false(is_matrix(list(1:3)))
  expect_false(is_matrix(data.frame(x = LETTERS, y = 1:26)))

  expect_false(is_matrix(matrix(1:3), n_col = 2))
  expect_false(is_matrix(matrix(1:4, nrow = 2), n_row = 3))

})

# TEST: is_data_frame ----------------------------------------------------------

test_that("is_data_frame returns TRUE for valid inputs", {

  expect_true(is_data_frame(data.frame(x = LETTERS, y = 1:26)))

  expect_true(is_data_frame(data.frame(x = LETTERS, y = 1:26), n_col = 2))
  expect_true(is_data_frame(data.frame(x = LETTERS, y = 1:26), n_row = 26))

})

test_that("is_data_frame returns FALSE for invalid inputs", {

  expect_false(is_data_frame(TRUE))
  expect_false(is_data_frame(1:3))
  expect_false(is_data_frame(c(1.1, 2.2, 3.3)))
  expect_false(is_data_frame(matrix(1:4, nrow = 2)))
  expect_false(is_data_frame("bob"))
  expect_false(is_data_frame(list(1:3)))

  expect_false(is_data_frame(data.frame(x = LETTERS, y = 1:26), n_col = 1))
  expect_false(is_data_frame(data.frame(x = LETTERS, y = 1:26), n_row = 10))

})

# TEST: has_names --------------------------------------------------------------

test_that("has_names returns TRUE for an object with names", {

  expect_true(has_names(c(a = 1, b = 2)))
  expect_true(has_names(list(a = 1, b = 2)))
  expect_true(has_names(data.frame(a = 1, b = 2)))

  expect_true(has_names(c(a = 1, b = 2), "a"))
  expect_identical(
    has_names(list(a = 1, b = 2), c("a", "b")),
    c(TRUE, TRUE)
  )
  expect_true(has_names(data.frame(a = 1, b = 2), "a"))

})

test_that("has_names returns FALSE for an object without names", {

  expect_false(has_names(1:3))
  expect_false(has_names("bob"))
  expect_false(has_names(list(1:3)))

  expect_false(has_names(c(a = 1, b = 2), "c"))
  expect_false(has_names(data.frame(a = 1, b = 2), "c"))

  expect_false(has_names(1:3, "a"))
  expect_false(has_names("bob", "a"))
  expect_false(has_names(list(1:3), "a"))

  expect_identical(
    has_names(list(a = 1, b = 2), c("b", "c")),
    c(TRUE, FALSE)
  )

})

# TEST: has_length -------------------------------------------------------------

test_that("has_length returns TRUE for valid inputs", {

  expect_true(has_length(1, 1))
  expect_true(has_length(c("bob", "jane"), 2))
  expect_true(has_length(list(x = 1, y = 2, z = 3), 3))

  expect_true(has_length(1:3, n_min = 2))
  expect_true(has_length(1:3, n_max = 10))

  expect_true(has_length(1:3, n_min = 2, n_max = 10))

})

test_that("has_length returns FALSE for invalid inputs", {

  expect_false(has_length(1:3, 1))
  expect_false(has_length(c("bob", "jane"), 3))
  expect_false(has_length(list(x = 1, y = 2, z = 3), 5))

  expect_false(has_length("A", n_min = 2))
  expect_false(has_length(LETTERS, n_max = 10))

  expect_false(has_length(1, n_min = 2, n_max = 10))
  expect_false(has_length(1:11, n_min = 2, n_max = 10))

})

# TEST: is_na ------------------------------------------------------------------

test_that("is_na returns TRUE for NA", {

  expect_true(is_na(NA))
  expect_true(is_na(NA_integer_))
  expect_true(is_na(NA_real_))
  expect_true(is_na(NA_complex_))
  expect_true(is_na(NA_character_))

  expect_identical(is_na(c(1, NA, 3)), c(FALSE, TRUE, FALSE))
  expect_identical(
    is_na(matrix(c(1, 2, NA, 4), nrow = 2)),
    matrix(c(FALSE, FALSE, TRUE, FALSE), nrow = 2)
  )
  expect_identical(
    is_na(data.frame(x = LETTERS[1:3], y = c(1, NA, 3))),
    matrix(
      c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE),
      nrow     = 3,
      dimnames = list(NULL, c("x", "y"))
    )
  )

})

test_that("is_na returns FALSE for non-NAs", {

  expect_false(is_na(TRUE))
  expect_false(is_na(1L))
  expect_false(is_na(3.142))
  expect_false(is_na("bob"))

  expect_false(is_na(NULL))
  expect_false(is_na(list(1:3)))

})

# TEST: is_in ------------------------------------------------------------------

test_that("is_in returns TRUE if elements of x are in allowed values", {

  expect_true(is_in(1, 1:3))
  expect_identical(
    is_in(c("a", "b"), letters),
    c(TRUE, TRUE)
  )
  expect_identical(
    is_in(list("a", "b"), as.list(letters)),
    c(TRUE, TRUE)
  )

})

test_that("is_in returns FALSE if elements of x are not in allowed values", {

  expect_false(is_in(0, 1:3))
  expect_identical(
    is_in(c("A", "D"), c("A", "B", "C")),
    c(TRUE, FALSE)
  )
  expect_false(is_in(options, "msgr.level"))

})

# TEST: is_in_range ------------------------------------------------------------

test_that("is_in_range returns TRUE if elements of x are in range", {

  expect_true(is_in_range(1, min = 0))
  expect_true(is_in_range(1, max = 10))
  expect_true(is_in_range(1, min = 0, max = 10))

  expect_identical(
    is_in_range(c(1.1, 2.2, 3.3), min = 0),
    c(TRUE, TRUE, TRUE)
  )
  expect_identical(
    is_in_range(c(1.1, 2.2, 3.3), max = 10),
    c(TRUE, TRUE, TRUE)
  )
  expect_identical(
    is_in_range(c(1.1, 2.2, 3.3), min = 0, max = 10),
    c(TRUE, TRUE, TRUE)
  )

})

test_that("is_in_range returns FALSE if the elements of x are not in range", {

  expect_false(is_in_range(0, min = 1))
  expect_false(is_in_range(11, max = 10))
  expect_false(is_in_range(0, min = 1, max = 10))
  expect_false(is_in_range(11, min = 1, max = 10))

  expect_identical(
    is_in_range(c(1.1, 2.2, 3.3), min = 10),
    c(FALSE, FALSE, FALSE)
  )
  expect_identical(
    is_in_range(c(1.1, 2.2, 3.3), max = 1),
    c(FALSE, FALSE, FALSE)
  )
  expect_identical(
    is_in_range(c(1.1, 2.2, 3.3), min = 2, max = 3),
    c(FALSE, TRUE, FALSE)
  )

  expect_false(is_in_range("a", min = 0))

})

# TEST: has_char_length --------------------------------------------------------

test_that("has_char_length returns TRUE if character length is valid", {

  expect_true(has_char_length("", 0))
  expect_true(has_char_length("sometext", 8))
  expect_true(has_char_length("sometext", n_min = 1))
  expect_true(has_char_length("sometext", n_max = 10))
  expect_true(has_char_length("sometext", n_min = 1, n_max = 10))

  expect_identical(
    has_char_length(c("some", "text"), 4),
    c(TRUE, TRUE)
  )
  expect_identical(
    has_char_length(c("some", "text"), n_min = 1),
    c(TRUE, TRUE)
  )
  expect_identical(
    has_char_length(c("some", "text"), n_max = 10),
    c(TRUE, TRUE)
  )
  expect_identical(
    has_char_length(c("some", "text"), n_min = 1, n_max = 10),
    c(TRUE, TRUE)
  )

})

test_that("has_char_length returns FALSE if character length is invalid", {

  expect_false(has_char_length(1, 0))
  expect_false(has_char_length("sometext", 4))
  expect_false(has_char_length("sometext", n_min = 10))
  expect_false(has_char_length("sometext", n_max = 4))
  expect_false(has_char_length("", n_min = 1, n_max = 10))
  expect_false(has_char_length("somemoretext", n_min = 1, n_max = 10))

  expect_identical(
    has_char_length(c("different", "text"), 4),
    c(FALSE, TRUE)
  )
  expect_identical(
    has_char_length(c("different", "text"), n_min = 5),
    c(TRUE, FALSE)
  )
  expect_identical(
    has_char_length(c("different", "text"), n_max = 5),
    c(FALSE, TRUE)
  )
  expect_identical(
    has_char_length(c("different", "text"), n_min = 5, n_max = 8),
    c(FALSE, FALSE)
  )

})

# TEST: is_file ----------------------------------------------------------------

test_that("is_file returns TRUE for an existing directory", {

  temp_file_1 <- tempfile("1", fileext = ".txt")
  temp_file_2 <- tempfile("2", fileext = ".txt")

  file.create(temp_file_1)
  file.create(temp_file_2)

  expect_true(is_file(temp_file_1))
  expect_true(is_file(temp_file_2))

  expect_identical(
    is_file(c(temp_file_1, temp_file_2)),
    c(TRUE, TRUE)
  )

})

test_that("is_file returns FALSE for an invalid directory", {

  temp_file <- tempfile(fileext = ".txt")

  no_file <- file.path(tempdir(), "does_not_exist.txt")
  file.create(temp_file)

  expect_false(is_file(TRUE))
  expect_false(is_file(3.142))
  expect_false(is_file("bob"))
  expect_false(is_file(list(1:3)))
  expect_false(is_file(matrix(1:4, nrow = 2)))
  expect_false(is_file(data.frame(x = LETTERS, y = 1:26)))

  expect_false(is_file("C:/does/not/exist.txt"))
  expect_false(is_file(no_file))

  expect_identical(
    is_file(c(temp_file, no_file)),
    c(TRUE, FALSE)
  )

})

# TEST: is_dir -----------------------------------------------------------------

test_that("is_dir returns TRUE for an existing directory", {

  home_dir <- R.home()
  temp_dir <- tempdir()

  expect_true(is_dir(home_dir))
  expect_true(is_dir(temp_dir))

  expect_identical(
    is_dir(c(home_dir, temp_dir)),
    c(TRUE, TRUE)
  )

})

test_that("is_dir returns FALSE for an invalid directory", {

  temp_dir <- tempdir()
  no_dir   <- file.path(temp_dir, "does_not_exist")

  expect_false(is_dir(TRUE))
  expect_false(is_dir(3.142))
  expect_false(is_dir("bob"))
  expect_false(is_dir(list(1:3)))
  expect_false(is_dir(matrix(1:4, nrow = 2)))
  expect_false(is_dir(data.frame(x = LETTERS, y = 1:26)))

  expect_false(is_dir("C:/does/not/exist"))
  expect_false(is_dir(no_dir))

  expect_identical(
    is_dir(c(temp_dir, no_dir)),
    c(TRUE, FALSE)
  )

})

# TEST: is_readable ------------------------------------------------------------

test_that("is_readable returns TRUE for a readable file or directory", {

  temp_dir <- tempdir()

  temp_file <- file.path(temp_dir, "readable_file.txt")
  file.create(temp_file)

  expect_true(is_readable(temp_dir))
  expect_true(is_readable(temp_file))

  expect_identical(
    is_readable(c(temp_dir, temp_file)),
    c(TRUE, TRUE)
  )

})

test_that("is_readable returns FALSE for an unreadable file or directory", {

  temp_dir <- tempdir()
  no_dir <- file.path(temp_dir, "does_not_exist")

  temp_file <- file.path(temp_dir, "unreadable_file.txt")

  expect_false(is_readable(TRUE))
  expect_false(is_readable(3.142))
  expect_false(is_readable("bob"))
  expect_false(is_readable(list(1:3)))
  expect_false(is_readable(matrix(1:4, nrow = 2)))
  expect_false(is_readable(data.frame(x = LETTERS, y = 1:26)))

  expect_false(is_readable("C:/does/not/exist"))
  expect_false(is_readable(no_dir))
  expect_false(is_readable(temp_file))

  expect_identical(
    is_readable(c(temp_dir, no_dir)),
    c(TRUE, FALSE)
  )

})

# TEST: is_writeable -----------------------------------------------------------

test_that("is_writeable returns TRUE for a readable file or directory", {

  temp_dir <- tempdir()

  temp_file <- file.path(temp_dir, "writeable_file.txt")
  file.create(temp_file)

  expect_true(is_writeable(temp_dir))
  expect_true(is_writeable(temp_file))

  expect_identical(
    is_readable(c(temp_dir, temp_file)),
    c(TRUE, TRUE)
  )

})

test_that("is_writeable returns FALSE for an unreadable file or directory", {

  temp_dir <- tempdir()
  no_dir <- file.path(temp_dir, "does_not_exist")

  temp_file <- file.path(temp_dir, "unwriteable_file.txt")

  expect_false(is_writeable(TRUE))
  expect_false(is_writeable(3.142))
  expect_false(is_writeable("bob"))
  expect_false(is_writeable(list(1:3)))
  expect_false(is_writeable(matrix(1:4, nrow = 2)))
  expect_false(is_writeable(data.frame(x = LETTERS, y = 1:26)))

  expect_false(is_writeable("C:/does/not/exist"))
  expect_false(is_writeable(no_dir))
  expect_false(is_writeable(temp_file))

  expect_identical(
    is_readable(c(temp_dir, no_dir)),
    c(TRUE, FALSE)
  )

})

# TEST: is_url -----------------------------------------------------------------

test_that("is_url returns TRUE for valid URLs", {

  expect_true(is_url("https://www.secure.com"))
  expect_true(is_url("http://www.insecure.com"))
  expect_identical(
    is_url(c("http://www.insecure.com", "http://www.insecure.com")),
    c(TRUE, TRUE)
  )

  expect_true(is_url("https://www.secure.com/some/endpoint"))
  expect_true(is_url("http://www.insecure.com/some/endpoint"))

})

test_that("is_url returns FALSE for invalid URLs", {

  expect_false(is_url(TRUE))
  expect_false(is_url(3.142))
  expect_false(is_url("bob"))
  expect_false(is_url(list(1:3)))
  expect_false(is_url(matrix(1:4, nrow = 2)))
  expect_false(is_url(data.frame(x = LETTERS, y = 1:26)))

  expect_identical(
    is_url(c("https://www.secure.com/endpoint","bob")),
    c(TRUE, FALSE)
  )

})
