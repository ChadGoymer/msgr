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

# TEST: assert_type ------------------------------------------------------------

test_that("assert_type returns an error if input is invalid", {

  expect_silent(assert_type(data.frame(x = 1:3), "list"))
  expect_silent(assert_type(list(1, 2, 3), "list", n = 3))

  expect_error(
    assert_type(1:3, "list"),
    "'1:3' must have type 'list'"
  )
  expect_error(
    assert_type(1:3, "integer", n = 1),
    "'1:3' must have type 'integer' and of length 1"
  )

  test_assert <- function(var, len = NULL)
    assert_type(var, "list", n = len)

  expect_silent(test_assert(data.frame(x = 1:3)))
  expect_silent(test_assert(list(1, 2, 3), len = 3))

  expect_error(
    test_assert(1:3),
    "In test_assert\\(\\): 'var' must have type 'list'"
  )
  expect_error(
    test_assert(list(1, 2, 3), len = 1),
    "In test_assert\\(\\): 'var' must have type 'list' and of length 1"
  )

})

# TEST: assert_class -----------------------------------------------------------

test_that("assert_class returns an error if input is invalid", {

  expect_silent(assert_class(data.frame(x = 1:3), "data.frame"))
  expect_silent(assert_class(list(1, 2, 3), "list", n = 3))

  expect_error(
    assert_class(list(x = 1:3), "data.frame"),
    "'list\\(x = 1:3\\)' must have class 'data.frame'"
  )
  expect_error(
    assert_class(1:3, "integer", n = 1),
    "'1:3' must have class 'integer' and of length 1"
  )

  test_assert <- function(var, len = NULL)
    assert_class(var, "list", n = len)

  expect_silent(test_assert(list(1, 2, 3)))
  expect_silent(test_assert(list(1, 2, 3), len = 3))

  expect_error(
    test_assert(1:3),
    "In test_assert\\(\\): 'var' must have class 'list'"
  )
  expect_error(
    test_assert(list(1, 2, 3), len = 1),
    "In test_assert\\(\\): 'var' must have class 'list' and of length 1"
  )

})

# TEST: assert_null ------------------------------------------------------------

test_that("assert_null returns an error if input is invalid", {

  expect_silent(assert_null(NULL))
  expect_error(
    assert_null(1),
    "'1' must be NULL"
  )

  test_assert <- function(var) assert_null(var)

  expect_silent(test_assert(NULL))
  expect_error(
    test_assert(1),
    "In test_assert\\(\\): 'var' must be NULL"
  )

})

# TEST: assert_atomic ----------------------------------------------------------

test_that("assert_atomic returns an error if input is invalid", {

  expect_silent(assert_atomic(1:3))
  expect_silent(assert_atomic(1:3, n = 3))

  expect_error(
    assert_atomic(list(1)),
    "'list\\(1\\)' must be an atomic vector"
  )
  expect_error(
    assert_atomic(1:3, n = 1),
    "'1:3' must be an atomic vector of length 1"
  )

  test_assert <- function(var, len = NULL)
    assert_atomic(x = var, n = len)

  expect_silent(test_assert(1:3))
  expect_error(
    test_assert(list(1)),
    "In test_assert\\(\\): 'var' must be an atomic vector"
  )

  expect_silent(test_assert(1:3, len = 3))
  expect_error(
    test_assert(1:3, len = 1),
    "In test_assert\\(\\): 'var' must be an atomic vector of length 1"
  )

})

# TEST: assert_vector ----------------------------------------------------------

test_that("assert_vector returns an error if input is invalid", {

  expect_silent(assert_vector(1:3))
  expect_silent(assert_vector(1:3, n = 3))

  expect_error(
    assert_vector(options),
    "'options' must be a vector"
  )
  expect_error(
    assert_vector(1:3, n = 1),
    "'1:3' must be a vector of length 1"
  )

  test_assert <- function(var, len = NULL)
    assert_vector(x = var, n = len)

  expect_silent(test_assert(1:3))
  expect_silent(test_assert(1:3, len = 3))

  expect_error(
    test_assert(options),
    "In test_assert\\(\\): 'var' must be a vector"
  )
  expect_error(
    test_assert(1:3, len = 1),
    "In test_assert\\(\\): 'var' must be a vector of length 1"
  )

})

# TEST: assert_logical ---------------------------------------------------------

test_that("assert_logical returns an error if input is invalid", {

  expect_silent(assert_logical(c(TRUE, FALSE, TRUE)))
  expect_silent(assert_logical(c(TRUE, FALSE, TRUE), n = 3))

  expect_error(
    assert_logical(1),
    "'1' must be a logical vector"
  )
  expect_error(
    assert_logical(c(TRUE, FALSE, TRUE), n = 1),
    "'c\\(TRUE, FALSE, TRUE\\)' must be a logical vector of length 1"
  )

  test_assert <- function(var, len = NULL)
    assert_logical(x = var, n = len)

  expect_silent(test_assert(c(TRUE, FALSE, TRUE)))
  expect_silent(test_assert(c(TRUE, FALSE, TRUE), len = 3))

  expect_error(
    test_assert(1),
    "In test_assert\\(\\): 'var' must be a logical vector"
  )
  expect_error(
    test_assert(c(TRUE, FALSE, TRUE), len = 1),
    "In test_assert\\(\\): 'var' must be a logical vector of length 1"
  )

})

# TEST: assert_integer ---------------------------------------------------------

test_that("assert_integer returns an error if input is invalid", {

  expect_silent(assert_integer(1:3))
  expect_silent(assert_integer(1:3, n = 3))

  expect_error(
    assert_integer(c(1.414, 3.142)),
    "'c\\(1.414, 3.142\\)' must be an integer vector"
  )
  expect_error(
    assert_integer(1:3, n = 1),
    "'1:3' must be an integer vector of length 1"
  )

  test_assert <- function(var, len = NULL)
    assert_integer(x = var, n = len)

  expect_silent(test_assert(1:3))
  expect_silent(test_assert(1:3, len = 3))

  expect_error(
    test_assert(c(1.414, 3.142)),
    "In test_assert\\(\\): 'var' must be an integer vector"
  )
  expect_error(
    test_assert(1:3, len = 1),
    "In test_assert\\(\\): 'var' must be an integer vector of length 1"
  )

})

# TEST: assert_natural ---------------------------------------------------------

test_that("assert_natural returns an error if input is invalid", {

  expect_silent(assert_natural(c(1.0, 2.0, 3.0)))
  expect_silent(assert_natural(c(1.0, 2.0, 3.0), n = 3))

  expect_error(
    assert_natural(-1:3),
    "'-1:3' must be a positive integer vector"
  )
  expect_error(
    assert_natural(c(1.414, 3.142)),
    "'c\\(1.414, 3.142\\)' must be a positive integer vector"
  )
  expect_error(
    assert_natural(1:3, n = 1),
    "'1:3' must be a positive integer vector of length 1"
  )

  test_assert <- function(var, len = NULL)
    assert_natural(x = var, n = len)

  expect_silent(test_assert(c(1.0, 2.0, 3.0)))
  expect_silent(test_assert(c(1.0, 2.0, 3.0), len = 3))

  expect_error(
    test_assert(c(1.414, 3.142)),
    "In test_assert\\(\\): 'var' must be a positive integer vector"
  )
  expect_error(
    test_assert(1:3, len = 1),
    "In test_assert\\(\\): 'var' must be a positive integer vector of length 1"
  )

})

# TEST: assert_double ----------------------------------------------------------

test_that("assert_double returns an error if input is invalid", {

  expect_silent(assert_double(c(1.1, 2.2, 3.3)))
  expect_silent(assert_double(c(1.1, 2.2, 3.3), n = 3))

  expect_error(
    assert_double(c("a", "b", "c")),
    "'c\\(\"a\", \"b\", \"c\"\\)' must be a double vector"
  )
  expect_error(
    assert_double(c(1.1, 2.2, 3.3), n = 1),
    "'c\\(1.1, 2.2, 3.3\\)' must be a double vector of length 1"
  )

  test_assert <- function(var, len = NULL)
    assert_double(x = var, n = len)

  expect_silent(test_assert(c(1.0, 2.0, 3.0)))
  expect_silent(test_assert(c(1.0, 2.0, 3.0), len = 3))

  expect_error(
    test_assert(c("a", "b", "c")),
    "In test_assert\\(\\): 'var' must be a double vector"
  )
  expect_error(
    test_assert(c(1.1, 2.2, 3.3), len = 1),
    "In test_assert\\(\\): 'var' must be a double vector of length 1"
  )

})

# TEST: assert_number ----------------------------------------------------------

test_that("assert_number returns an error if input is invalid", {

  expect_silent(assert_number(1:3))
  expect_silent(assert_number(c(1.1, 2.2, 3.3), n = 3))

  expect_error(
    assert_number(c("a", "b", "c")),
    "'c\\(\"a\", \"b\", \"c\"\\)' must be a numeric vector"
  )
  expect_error(
    assert_number(c(1.1, 2.2, 3.3), n = 1),
    "'c\\(1.1, 2.2, 3.3\\)' must be a numeric vector of length 1"
  )

  test_assert <- function(var, len = NULL)
    assert_number(x = var, n = len)

  expect_silent(test_assert(1:3))
  expect_silent(test_assert(c(1.1, 2.2, 3.3), len = 3))

  expect_error(
    test_assert(c("a", "b", "c")),
    "In test_assert\\(\\): 'var' must be a numeric vector"
  )
  expect_error(
    test_assert(c(1.1, 2.2, 3.3), len = 1),
    "In test_assert\\(\\): 'var' must be a numeric vector of length 1"
  )

})

# TEST: assert_character -------------------------------------------------------

test_that("assert_character returns an error if input is invalid", {

  expect_silent(assert_character(letters))
  expect_silent(assert_character(c("a", "b", "c"), n = 3))

  expect_error(
    assert_character(1:3),
    "'1:3' must be a character vector"
  )
  expect_error(
    assert_character(letters, n = 1),
    "'letters' must be a character vector of length 1"
  )

  test_assert <- function(var, len = NULL)
    assert_character(x = var, n = len)

  expect_silent(test_assert(letters))
  expect_silent(test_assert(c("a", "b", "c"), len = 3))

  expect_error(
    test_assert(1:3),
    "In test_assert\\(\\): 'var' must be a character vector"
  )
  expect_error(
    test_assert(letters, len = 1),
    "In test_assert\\(\\): 'var' must be a character vector of length 1"
  )

})

# TEST: assert_factor -------------------------------------------------------

test_that("assert_factor returns an error if input is invalid", {

  test_fct <- factor(c("a", "b", "a"))
  expect_silent(assert_factor(test_fct))
  expect_silent(assert_factor(test_fct, levels = c("a", "b")))
  expect_silent(assert_factor(test_fct, n = 3))
  expect_silent(assert_factor(test_fct, levels = c("a", "b"), n = 3))

  expect_error(
    assert_factor(1:3),
    "'1:3' must be a factor vector"
  )
  expect_error(
    assert_factor(test_fct, levels = c("b", "c")),
    "'test_fct' must be a factor vector with levels 'b', 'c'"
  )
  expect_error(
    assert_factor(test_fct, n = 1),
    "'test_fct' must be a factor vector of length 1"
  )
  expect_error(
    assert_factor(test_fct, levels = c("b", "c"), n = 1),
    "'test_fct' must be a factor vector with levels 'b', 'c' of length 1"
  )

  test_assert <- function(var, lev = NULL, len = NULL)
    assert_factor(x = var, levels = lev, n = len)

  expect_silent(test_assert(test_fct))
  expect_silent(test_assert(test_fct, lev = c("a", "b")))
  expect_silent(test_assert(test_fct, len = 3))
  expect_silent(test_assert(test_fct, lev = c("a", "b"), len = 3))

  expect_error(
    test_assert(1:3),
    "In test_assert\\(\\): 'var' must be a factor vector"
  )
  expect_error(
    test_assert(letters, lev = c("b", "c")),
    "In test_assert\\(\\): 'var' must be a factor vector with levels 'b', 'c'"
  )
  expect_error(
    test_assert(letters, len = 1),
    "In test_assert\\(\\): 'var' must be a factor vector of length 1"
  )
  expect_error(
    test_assert(letters, lev = c("b", "c"), len = 1),
    paste(
      "In test_assert\\(\\): 'var' must be a factor vector",
      "with levels 'b', 'c' of length 1"
    )
  )

})

# TEST: assert_list ------------------------------------------------------------

test_that("assert_list returns an error if input is invalid", {

  expect_silent(assert_list(list(1, 2, 3)))
  expect_silent(assert_list(list(1, 2, 3), n = 3))

  expect_error(
    assert_list(1:3),
    "'1:3' must be a list"
  )
  expect_error(
    assert_list(list(1, 2, 3), n = 1),
    "'list\\(1, 2, 3\\)' must be a list of length 1"
  )

  test_assert <- function(var, len = NULL)
    assert_list(x = var, n = len)

  expect_silent(test_assert(list(1, 2, 3)))
  expect_silent(test_assert(list(1, 2, 3), len = 3))

  expect_error(
    test_assert(1:3),
    "In test_assert\\(\\): 'var' must be a list"
  )
  expect_error(
    test_assert(list(1, 2, 3), len = 1),
    "In test_assert\\(\\): 'var' must be a list of length 1"
  )

})

# TEST: assert_array -----------------------------------------------------------

test_that("assert_array returns an error if input is invalid", {

  test_ary <- array(1:12, dim = c(3, 4))
  expect_silent(assert_array(test_ary))
  expect_silent(assert_array(test_ary, dims = c(3, 4)))

  expect_error(
    assert_array(1:3),
    "'1:3' must be an array"
  )
  expect_error(
    assert_array(test_ary, dims = c(1, 1)),
    "'test_ary' must be an array with dimensions 1 x 1"
  )

  test_assert <- function(var, dim = NULL)
    assert_array(x = var, dims = dim)

  expect_silent(test_assert(test_ary))
  expect_silent(test_assert(test_ary, dim = c(3, 4)))

  expect_error(
    test_assert(1:3),
    "In test_assert\\(\\): 'var' must be an array"
  )
  expect_error(
    test_assert(test_ary, dim = c(1, 1)),
    "In test_assert\\(\\): 'var' must be an array with dimensions 1 x 1"
  )

})

# TEST: assert_matrix ----------------------------------------------------------

test_that("assert_matrix returns an error if input is invalid", {

  test_mtx <- matrix(1:12, ncol = 4, nrow = 3)
  expect_silent(assert_matrix(test_mtx))
  expect_silent(assert_matrix(test_mtx, n_col = 4))
  expect_silent(assert_matrix(test_mtx, n_row = 3))
  expect_silent(assert_matrix(test_mtx, n_col = 4, n_row = 3))

  expect_error(
    assert_matrix(1:3),
    "'1:3' must be a matrix"
  )
  expect_error(
    assert_matrix(test_mtx, n_col = 1),
    "'test_mtx' must be a matrix with 1 columns"
  )
  expect_error(
    assert_matrix(test_mtx, n_row = 1),
    "'test_mtx' must be a matrix with 1 rows"
  )
  expect_error(
    assert_matrix(test_mtx, n_col = 1, n_row = 1),
    "'test_mtx' must be a matrix with 1 columns and 1 rows"
  )

  test_assert <- function(var, col = NULL, row = NULL)
    assert_matrix(x = var, n_col = col, n_row = row)

  expect_silent(test_assert(test_mtx))
  expect_silent(test_assert(test_mtx, col = 4))
  expect_silent(test_assert(test_mtx, row = 3))
  expect_silent(test_assert(test_mtx, col = 4, row = 3))

  expect_error(
    test_assert(1:3),
    "In test_assert\\(\\): 'var' must be a matrix"
  )
  expect_error(
    test_assert(test_mtx, col = 1),
    "In test_assert\\(\\): 'var' must be a matrix with 1 columns"
  )
  expect_error(
    test_assert(test_mtx, row = 1),
    "In test_assert\\(\\): 'var' must be a matrix with 1 rows"
  )
  expect_error(
    test_assert(test_mtx, col = 1, row = 1),
    "In test_assert\\(\\): 'var' must be a matrix with 1 columns and 1 rows"
  )

})

# TEST: assert_data_frame ------------------------------------------------------

test_that("assert_data_frame returns an error if input is invalid", {

  test_df <- data.frame(a = 1:3, b = c("a", "b", "c"))
  expect_silent(assert_data_frame(test_df))
  expect_silent(assert_data_frame(test_df, n_col = 2))
  expect_silent(assert_data_frame(test_df, n_row = 3))
  expect_silent(assert_data_frame(test_df, n_col = 2, n_row = 3))

  expect_error(
    assert_data_frame(1:3),
    "'1:3' must be a data.frame"
  )
  expect_error(
    assert_data_frame(test_df, n_col = 1),
    "'test_df' must be a data.frame with 1 columns"
  )
  expect_error(
    assert_data_frame(test_df, n_row = 1),
    "'test_df' must be a data.frame with 1 rows"
  )
  expect_error(
    assert_data_frame(test_df, n_col = 1, n_row = 1),
    "'test_df' must be a data.frame with 1 columns and 1 rows"
  )

  test_assert <- function(var, col = NULL, row = NULL)
    assert_data_frame(x = var, n_col = col, n_row = row)

  expect_silent(test_assert(test_df))
  expect_silent(test_assert(test_df, col = 2))
  expect_silent(test_assert(test_df, row = 3))
  expect_silent(test_assert(test_df, col = 2, row = 3))

  expect_error(
    test_assert(1:3),
    "In test_assert\\(\\): 'var' must be a data.frame"
  )
  expect_error(
    test_assert(test_df, col = 1),
    "In test_assert\\(\\): 'var' must be a data.frame with 1 columns"
  )
  expect_error(
    test_assert(test_df, row = 1),
    "In test_assert\\(\\): 'var' must be a data.frame with 1 rows"
  )
  expect_error(
    test_assert(test_df, col = 1, row = 1),
    "In test_assert\\(\\): 'var' must be a data.frame with 1 columns and 1 rows"
  )

})

# TEST: assert_function --------------------------------------------------------

test_that("assert_function returns an error if input is invalid", {

  expect_silent(assert_function(options))

  expect_error(
    assert_function(1:3),
    "'1:3' must be a function"
  )

  test_assert <- function(var)
    assert_function(x = var)

  expect_silent(test_assert(options))

  expect_error(
    test_assert(1:3),
    "In test_assert\\(\\): 'var' must be a function"
  )

})

# TEST: assert_formula ---------------------------------------------------------

test_that("assert_formula returns an error if input is invalid", {

  expect_silent(assert_formula(x ~ y))

  expect_error(
    assert_formula(1:3),
    "'1:3' must be a formula"
  )

  test_assert <- function(var)
    assert_formula(x = var)

  expect_silent(test_assert(x ~ y))

  expect_error(
    test_assert(1:3),
    "In test_assert\\(\\): 'var' must be a formula"
  )

})

# TEST: assert_empty -----------------------------------------------------------

test_that("assert_empty returns an error if input is invalid", {

  expect_silent(assert_empty(NULL))
  expect_silent(assert_empty(integer()))

  expect_error(
    assert_empty(1:3),
    "'1:3' must be empty or 'NULL'"
  )

  test_assert <- function(var)
    assert_empty(x = var)

  expect_silent(test_assert(NULL))
  expect_silent(test_assert(character()))

  expect_error(
    test_assert(1:3),
    "In test_assert\\(\\): 'var' must be empty or 'NULL'"
  )

})

# TEST: assert_names -----------------------------------------------------------

test_that("assert_names returns an error if input is invalid", {

  test_lst <- list(name = "Bob", age = 42)
  expect_silent(assert_names(test_lst))
  expect_silent(assert_names(test_lst, names = c("name", "age")))

  expect_error(
    assert_names(1:3),
    "'1:3' must have names"
  )
  expect_error(
    assert_names(test_lst, names = c("email", "address")),
    "'test_lst' must have names in 'email', 'address'"
  )

  test_assert <- function(var, nam = NULL)
    assert_names(x = var, names = nam)

  expect_silent(test_assert(test_lst))
  expect_silent(test_assert(test_lst, nam = c("name", "age")))

  expect_error(
    test_assert(1:3),
    "In test_assert\\(\\): 'var' must have names"
  )
  expect_error(
    test_assert(test_lst, nam = c("email", "address")),
    "In test_assert\\(\\): 'var' must have names in 'email', 'address'"
  )

})

# TEST: assert_length ----------------------------------------------------------

test_that("assert_length returns an error if input is invalid", {

  expect_silent(assert_length(1:3, n = 3))
  expect_silent(assert_length(1:3, n_min = 1))
  expect_silent(assert_length(1:3, n_max = 10))
  expect_silent(assert_length(1:3, n_min = 1, n_max = 10))

  expect_error(
    assert_length(1:3, n = 1),
    "'1:3' must have length == 1"
  )
  expect_error(
    assert_length(1:3, n_min = 5),
    "'1:3' must have length >= 5"
  )
  expect_error(
    assert_length(1:3, n_max = 2),
    "'1:3' must have length <= 2"
  )
  expect_error(
    assert_length(1:3, n_min = 5, n_max = 10),
    "'1:3' must have length >= 5 and <= 10"
  )

  test_assert <- function(var, len = NULL, min = NULL, max = NULL)
    assert_length(x = var, n = len, n_min = min, n_max = max)

  expect_silent(test_assert(1:3, len = 3))
  expect_silent(test_assert(1:3, min = 1))
  expect_silent(test_assert(1:3, max = 10))
  expect_silent(test_assert(1:3, min = 1, max = 10))

  expect_error(
    test_assert(1:3, len = 1),
    "'var' must have length == 1"
  )
  expect_error(
    test_assert(1:3, min = 5),
    "'var' must have length >= 5"
  )
  expect_error(
    test_assert(1:3, max = 2),
    "'var' must have length <= 2"
  )
  expect_error(
    test_assert(1:3, min = 5, max = 10),
    "'var' must have length >= 5 and <= 10"
  )

})

# TEST: assert_na --------------------------------------------------------------

test_that("assert_na returns an error if input is invalid", {

  expect_silent(assert_na(c(NA, NA)))
  expect_error(
    assert_na(1:3),
    "'1:3' must be a vector of NAs"
  )

  test_assert <- function(var)
    assert_na(x = var)

  expect_silent(test_assert(c(NA, NA)))
  expect_error(
    test_assert(1:3),
    "In test_assert\\(\\): 'var' must be a vector of NAs"
  )

})

# TEST: assert_in --------------------------------------------------------------

test_that("assert_in returns an error if input is invalid", {

  expect_silent(assert_in(1:3, values = 1:10))
  expect_silent(assert_in(c("A", "B"), values = LETTERS))

  expect_error(
    assert_in(1:10, values = 1:3),
    "'1:10' must be in '1', '2', '3'"
  )
  expect_error(
    assert_in(c("A", "B"), values = c("a", "b", "c")),
    "'c\\(\"A\", \"B\"\\)' must be in 'a', 'b', 'c'"
  )

  test_assert <- function(var, val = NULL)
    assert_in(x = var, values = val)

  expect_silent(test_assert(1:3, val = 1:10))
  expect_silent(test_assert(c("A", "B"), val = LETTERS))

  expect_error(
    test_assert(1:10, val = 1:3),
    "In test_assert\\(\\): 'var' must be in '1', '2', '3'"
  )
  expect_error(
    test_assert(c("A", "B"), val = c("a", "b", "c")),
    "In test_assert\\(\\): 'var' must be in 'a', 'b', 'c'"
  )

})

# TEST: assert_in_range --------------------------------------------------------

test_that("assert_in_range returns an error if input is invalid", {

  expect_silent(assert_in_range(1:3, min = 1))
  expect_silent(assert_in_range(1:3, max = 10))
  expect_silent(assert_in_range(1:3, min = 1, max = 10))

  expect_error(
    assert_in_range(1:3, min = 5),
    "'1:3' must have values >= 5"
  )
  expect_error(
    assert_in_range(1:3, max = 2),
    "'1:3' must have values <= 2"
  )
  expect_error(
    assert_in_range(1:3, min = 5, max = 10),
    "'1:3' must have values >= 5 and <= 10"
  )

  test_assert <- function(var, mn = NULL, mx = NULL)
    assert_in_range(x = var, min = mn, max = mx)

  expect_silent(test_assert(1:3, mn = 1))
  expect_silent(test_assert(1:3, mx = 10))
  expect_silent(test_assert(1:3, mn = 1, mx = 10))

  expect_error(
    test_assert(1:3, mn = 5),
    "'var' must have values >= 5"
  )
  expect_error(
    test_assert(1:3, mx = 2),
    "'var' must have values <= 2"
  )
  expect_error(
    test_assert(1:3, mn = 5, mx = 10),
    "'var' must have values >= 5 and <= 10"
  )

})

# TEST: assert_char_length -----------------------------------------------------

test_that("assert_char_length returns an error if input is invalid", {

  test_chr <- c("bob", "jan")
  expect_silent(assert_char_length(test_chr, n = 3))
  expect_silent(assert_char_length(test_chr, n_min = 1))
  expect_silent(assert_char_length(test_chr, n_max = 10))
  expect_silent(assert_char_length(test_chr, n_min = 1, n_max = 10))

  expect_error(
    assert_char_length(test_chr, n = 1),
    "'test_chr' must have character length == 1"
  )
  expect_error(
    assert_char_length(test_chr, n_min = 5),
    "'test_chr' must have character length >= 5"
  )
  expect_error(
    assert_char_length(test_chr, n_max = 2),
    "'test_chr' must have character length <= 2"
  )
  expect_error(
    assert_char_length(test_chr, n_min = 5, n_max = 10),
    "'test_chr' must have character length >= 5 and <= 10"
  )

  test_assert <- function(var, len = NULL, min = NULL, max = NULL)
    assert_char_length(x = var, n = len, n_min = min, n_max = max)

  expect_silent(test_assert(test_chr, len = 3))
  expect_silent(test_assert(test_chr, min = 1))
  expect_silent(test_assert(test_chr, max = 10))
  expect_silent(test_assert(test_chr, min = 1, max = 10))

  expect_error(
    test_assert(test_chr, len = 1),
    "'var' must have character length == 1"
  )
  expect_error(
    test_assert(test_chr, min = 5),
    "'var' must have character length >= 5"
  )
  expect_error(
    test_assert(test_chr, max = 2),
    "'var' must have character length <= 2"
  )
  expect_error(
    test_assert(test_chr, min = 5, max = 10),
    "'var' must have character length >= 5 and <= 10"
  )

})

# TEST: assert_file ------------------------------------------------------------

test_that("assert_file returns an error if input is invalid", {

  expect_silent(assert_file(fs::path_package("DESCRIPTION", package = "msgr")))
  expect_error(
    assert_file("no-such-file.txt"),
    "'\"no-such-file.txt\"' must be an existing file"
  )

  test_assert <- function(var)
    assert_file(x = var)

  expect_silent(test_assert(fs::path_package("DESCRIPTION", package = "msgr")))
  expect_error(
    test_assert("no-such-file.txt"),
    "In test_assert\\(\\): 'var' must be an existing file"
  )

})

# TEST: assert_dir -------------------------------------------------------------

test_that("assert_dir returns an error if input is invalid", {

  expect_silent(assert_dir(R.home()))
  expect_error(
    assert_dir("no-such-directory"),
    "'\"no-such-directory\"' must be an existing directory"
  )

  test_assert <- function(var)
    assert_dir(x = var)

  expect_silent(test_assert(R.home()))
  expect_error(
    test_assert("no-such-directory"),
    "In test_assert\\(\\): 'var' must be an existing directory"
  )

})

# TEST: assert_readable --------------------------------------------------------

test_that("assert_readable returns an error if input is invalid", {

  expect_silent(assert_readable(R.home()))
  expect_error(
    assert_readable("no-such-directory"),
    "'\"no-such-directory\"' must be a readable directory or file"
  )

  test_assert <- function(var)
    assert_readable(x = var)

  expect_silent(test_assert(R.home()))
  expect_error(
    test_assert("no-such-directory"),
    "In test_assert\\(\\): 'var' must be a readable directory or file"
  )

})

# TEST: assert_writeable -------------------------------------------------------

test_that("assert_writeable returns an error if input is invalid", {

  expect_silent(assert_writeable(Sys.getenv("HOME")))
  expect_error(
    assert_writeable("no-such-directory"),
    "'\"no-such-directory\"' must be a writeable directory or file"
  )

  test_assert <- function(var)
    assert_writeable(x = var)

  expect_silent(test_assert(Sys.getenv("HOME")))
  expect_error(
    test_assert("no-such-directory"),
    "In test_assert\\(\\): 'var' must be a writeable directory or file"
  )

})

# TEST: assert_url -------------------------------------------------------

test_that("assert_url returns an error if input is invalid", {

  expect_silent(assert_url("https://www.google.com"))
  expect_error(
    assert_url("no-such-site"),
    "'\"no-such-site\"' must be a valid URL"
  )

  test_assert <- function(var)
    assert_url(x = var)

  expect_silent(test_assert("https://www.google.com"))
  expect_error(
    test_assert("no-such-site"),
    "In test_assert\\(\\): 'var' must be a valid URL"
  )

})
