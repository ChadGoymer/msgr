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
  expect_false(is_url(data.frame(x = LETTERS[1:3], y = 1:3)))

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
  expect_false(is_dir(data.frame(x = LETTERS[1:3], y = 1:3)))

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
  expect_false(is_file(data.frame(x = LETTERS[1:3], y = 1:3)))

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
  expect_false(is_readable(data.frame(x = LETTERS[1:3], y = 1:3)))

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
  expect_false(is_writeable(data.frame(x = LETTERS[1:3], y = 1:3)))

  expect_false(is_writeable("C:/does/not/exist"))
  expect_false(is_writeable(no_dir))
  expect_false(is_writeable(temp_file))

  expect_identical(
    is_readable(c(temp_dir, no_dir)),
    c(TRUE, FALSE)
  )

})

# TEST: is_in ------------------------------------------------------------------

test_that("is_in returns TRUE if elements of x are in y", {

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

test_that("is_in returns FALSE if the elements of x are not in y", {

  expect_false(is_in(0, 1:3))
  expect_identical(
    is_in(c("A", "D"), c("A", "B", "C")),
    c(TRUE, FALSE)
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
