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

# TEST: is_integerish ----------------------------------------------------------

test_that("is_integerish returns TRUE for valid inputs", {

  expect_true(is_integerish(1L))
  expect_true(is_integerish(2.0))
  expect_true(is_integerish(1:3))
  expect_true(is_integerish(c(1.0, 2.0, 3.0)))
  expect_true(is_integerish(matrix(1:4, nrow = 2)))

})

test_that("is_integerish returns FALSE for invalid inputs", {

  expect_false(is_integerish(TRUE))
  expect_false(is_integerish(3.142))
  expect_false(is_integerish("bob"))
  expect_false(is_integerish(list(1:3)))
  expect_false(is_integerish(data.frame(x = LETTERS, y = 1:26)))

})

# TEST: is_scalar_integerish ---------------------------------------------------

test_that("is_scalar_integerish returns TRUE for valid inputs", {

  expect_true(is_scalar_integerish(1L))
  expect_true(is_scalar_integerish(2.0))

})

test_that("is_scalar_integerish returns FALSE for invalid inputs", {

  expect_false(is_scalar_integerish(TRUE))
  expect_false(is_scalar_integerish(3.142))
  expect_false(is_scalar_integerish("bob"))
  expect_false(is_scalar_integerish(list(1:3)))
  expect_false(is_scalar_integerish(data.frame(x = LETTERS, y = 1:26)))

  expect_false(is_scalar_integerish(1:3))
  expect_false(is_scalar_integerish(c(1.0, 2.0, 3.0)))
  expect_false(is_scalar_integerish(matrix(1:4, nrow = 2)))

})

# TEST: is_numeric -------------------------------------------------------------

test_that("is_numeric returns TRUE for valid inputs", {

  expect_true(is_numeric(1L))
  expect_true(is_numeric(2.5))
  expect_true(is_numeric(1:3))
  expect_true(is_numeric(c(1.1, 2.2, 3.3)))
  expect_true(is_numeric(matrix(1:4, nrow = 2)))

})

test_that("is_numeric returns FALSE for invalid inputs", {

  expect_false(is_numeric(TRUE))
  expect_false(is_numeric("bob"))
  expect_false(is_numeric(list(1:3)))
  expect_false(is_numeric(data.frame(x = LETTERS, y = 1:26)))

})

# TEST: is_scalar_numeric ------------------------------------------------------

test_that("is_scalar_numeric returns TRUE for valid inputs", {

  expect_true(is_scalar_numeric(1L))
  expect_true(is_scalar_numeric(2.2))

})

test_that("is_scalar_numeric returns FALSE for invalid inputs", {

  expect_false(is_scalar_numeric(TRUE))
  expect_false(is_scalar_numeric("bob"))
  expect_false(is_scalar_numeric(list(1:3)))
  expect_false(is_scalar_numeric(data.frame(x = LETTERS, y = 1:26)))

  expect_false(is_scalar_numeric(1:3))
  expect_false(is_scalar_numeric(c(1.1, 2.2, 3.3)))
  expect_false(is_scalar_numeric(matrix(1:4, nrow = 2)))

})

# TEST: is_factor --------------------------------------------------------------

test_that("is_factor returns TRUE for valid inputs", {

  expect_true(is_factor(factor(1, levels = 1)))
  expect_true(is_factor(factor(c("a", "b", "a"))))

})

test_that("is_factor returns FALSE for invalid inputs", {

  expect_false(is_factor(TRUE))
  expect_false(is_factor(1:3))
  expect_false(is_factor(c(1.1, 2.2, 3.3)))
  expect_false(is_factor(matrix(1:4, nrow = 2)))
  expect_false(is_factor("bob"))
  expect_false(is_factor(list(1:3)))
  expect_false(is_factor(data.frame(x = LETTERS, y = 1:26)))

})

# TEST: is_data_frame ----------------------------------------------------------

test_that("is_data_frame returns TRUE for valid inputs", {

  expect_true(is_data_frame(data.frame(x = LETTERS, y = 1:26)))

})

test_that("is_data_frame returns FALSE for invalid inputs", {

  expect_false(is_data_frame(TRUE))
  expect_false(is_data_frame(1:3))
  expect_false(is_data_frame(c(1.1, 2.2, 3.3)))
  expect_false(is_data_frame(matrix(1:4, nrow = 2)))
  expect_false(is_data_frame("bob"))
  expect_false(is_data_frame(list(1:3)))

})

# TEST: has_length -------------------------------------------------------------

test_that("has_length returns TRUE for valid inputs", {

  expect_true(has_length(1, 1))
  expect_true(has_length(c("bob", "jane"), 2))
  expect_true(has_length(list(x = 1, y = 2, z = 3), 3))

  expect_true(has_length(1:3, .min = 2))
  expect_true(has_length(1:3, .max = 10))

  expect_true(has_length(1:3, .min = 2, .max = 10))

})

test_that("has_length returns FALSE for invalid inputs", {

  expect_false(has_length(1:3, 1))
  expect_false(has_length(c("bob", "jane"), 3))
  expect_false(has_length(list(x = 1, y = 2, z = 3), 5))

  expect_false(has_length("A", .min = 2))
  expect_false(has_length(LETTERS, .max = 10))

  expect_false(has_length(1, .min = 2, .max = 10))
  expect_false(has_length(1:11, .min = 2, .max = 10))

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

})

# TEST: is_in_range ------------------------------------------------------------

test_that("is_in_range returns TRUE if elements of x are in range", {

  expect_true(is_in_range(1, .min = 0))
  expect_true(is_in_range(1, .max = 10))
  expect_true(is_in_range(1, .min = 0, .max = 10))

  expect_identical(
    is_in_range(c(1.1, 2.2, 3.3), .min = 0),
    c(TRUE, TRUE, TRUE)
  )
  expect_identical(
    is_in_range(c(1.1, 2.2, 3.3), .max = 10),
    c(TRUE, TRUE, TRUE)
  )
  expect_identical(
    is_in_range(c(1.1, 2.2, 3.3), .min = 0, .max = 10),
    c(TRUE, TRUE, TRUE)
  )

})

test_that("is_in_range returns FALSE if the elements of x are not in range", {

  expect_false(is_in_range(0, .min = 1))
  expect_false(is_in_range(11, .max = 10))
  expect_false(is_in_range(0, .min = 1, .max = 10))
  expect_false(is_in_range(11, .min = 1, .max = 10))

  expect_identical(
    is_in_range(c(1.1, 2.2, 3.3), .min = 10),
    c(FALSE, FALSE, FALSE)
  )
  expect_identical(
    is_in_range(c(1.1, 2.2, 3.3), .max = 1),
    c(FALSE, FALSE, FALSE)
  )
  expect_identical(
    is_in_range(c(1.1, 2.2, 3.3), .min = 2, .max = 3),
    c(FALSE, TRUE, FALSE)
  )

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

# TEST: has_char_length --------------------------------------------------------

test_that("has_char_length returns TRUE if character length is valid", {

  expect_true(has_char_length("", 0))
  expect_true(has_char_length("sometext", 8))
  expect_true(has_char_length("sometext", .min = 1))
  expect_true(has_char_length("sometext", .max = 10))
  expect_true(has_char_length("sometext", .min = 1, .max = 10))

  expect_identical(
    has_char_length(c("some", "text"), 4),
    c(TRUE, TRUE)
  )
  expect_identical(
    has_char_length(c("some", "text"), .min = 1),
    c(TRUE, TRUE)
  )
  expect_identical(
    has_char_length(c("some", "text"), .max = 10),
    c(TRUE, TRUE)
  )
  expect_identical(
    has_char_length(c("some", "text"), .min = 1, .max = 10),
    c(TRUE, TRUE)
  )

})

test_that("has_char_length returns FALSE if character length is invalid", {

  expect_false(has_char_length(1, 0))
  expect_false(has_char_length("sometext", 4))
  expect_false(has_char_length("sometext", .min = 10))
  expect_false(has_char_length("sometext", .max = 4))
  expect_false(has_char_length("", .min = 1, .max = 10))
  expect_false(has_char_length("somemoretext", .min = 1, .max = 10))

  expect_identical(
    has_char_length(c("different", "text"), 4),
    c(FALSE, TRUE)
  )
  expect_identical(
    has_char_length(c("different", "text"), .min = 5),
    c(TRUE, FALSE)
  )
  expect_identical(
    has_char_length(c("different", "text"), .max = 5),
    c(FALSE, TRUE)
  )
  expect_identical(
    has_char_length(c("different", "text"), .min = 5, .max = 8),
    c(FALSE, FALSE)
  )

})

