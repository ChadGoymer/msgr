# msgr 1.2.6

- Added `try_silently()`
- Added `with_msgr()`

# msgr 1.2.5

Simplified argument checks and package dependencies

# msgr 1.2.4

Added the following assertions on type:

- `assert_type()`: 'x"' must have type `type` and optionally length `n`.
- `assert_class()`: 'x"' must have class `class` and optionally length `n`.
- `assert_null()`: 'x' must be `NULL`
- `assert_atomic()`: 'x' must be an atomic vector and optionally length `n`.
- `assert_vector()`: 'x' must be a vector and optionally length `n`.
- `assert_logical()`: 'x' must be a logical vector and optionally length `n`.
- `assert_integer()`: 'x' must be an integer vector and optionally length `n`.
- `assert_natural()`: 'x' must be an integerish vector and optionally length 
  `n`.
- `assert_double()`: 'x' must be a double vector and optionally length `n`.
- `assert_number()`: 'x' must be a numeric vector and optionally length `n`.
- `assert_character()`: 'x' must be a character vector and optionally length 
  `n`.
- `assert_factor()`: 'x' must be a factor vector and optionally length `n`.
- `assert_list()`: 'x' must be a list and optionally length `n`.
- `assert_array()`: 'x' must be an array and optionally have dimension sizes 
  `dims`.
- `assert_matrix()`: 'x' must be a matrix and optionally have number of columns 
  `n_col` and number of rows `n_row`.
- `assert_data_frame()`: 'x' must be a data.frame and optionally have number of 
  columns `n_col` and number of rows `n_row`.
- `assert_function()`: 'x' must be a function
- `assert_formula()`: 'x' must be a formula

Added the following assertions on structure:

- `assert_empty()`: 'x' must be empty or `NULL`.
- `assert_names()`: 'x' must have names and optionally have the specified 
  `names`.
- `assert_length()`: 'x' must have valid length. You can specify the exact 
  length using `n` or the minimum and/or maximum length using `n_min` and
  `n_max` respectively.

Added the following assertions on values:

- `assert_na()`: All elements of 'x' must be `NA`.
- `assert_in()`: All elements of 'x' must in specified `values`.
- `assert_in_range()`: All elements of 'x' must be in the specified numeric
 range.
- `assert_char_length`: All elements of 'x' must have valid character length.
 You can specify the exact length using `n` or the minimum and/or maximum
 length using `n_min` and `n_max` respectively.

Added the following assertions on files:

- `assert_file()`: 'x' must be an existing file
- `assert_dir()`: 'x' must be an existing directory
- `assert_readable()`: 'x' must be a readable directory or file
- `assert_writeable()`: 'x' must be a writeable directory or file
- `assert_url()`: 'x' must be a valid URL

Fixed the range assertion on `msg_level`.

# msgr 1.2.3

Replaced functions that conflict with purrr:
- Replaced `is_integerish()` and `is_scalar_integerish()` with `is_natural()`
- Replaced `is_numeric()` and `is_scalar_numeric()` with `is_number()`

Added some new predicates:
- `is_type()`
- `is_class()`
- `is_array()`
- `is_matrix()`

Also tidied up documentation

# msgr 1.2.2

Added the following function to check type
- `is_integerish()`
- `is_scalar_integerish()`
- `is_numeric()`
- `is_scalar_numeric()`
- `is_factor()`
- `is_data_frame()`

Added or updated the following;
- `has_length()`
- `is_in()` (updated to be vectorised)
- `is_in_range()`
- `has_char_length()`

# msgr 1.2.1

- Do not override options if they are set before the package loads

# msgr 1.2.0

- Used `devtools::lint()` to follow tidyverse style guide

# msgr 1.1.3

- Using GitHub actions for CI

# msgr 1.1.2

- Added CRAN comments file

# msgr 1.1.1

- Added character check in `gh_map()` and `gh_pmap()`

# msgr 1.1.0

- Updated predicates and removed any that are in purrr
- Updated `try_map()` and `try_pmap()` to use `purrr::map()` and `purrr::pmap()`

# msgr 1.0.3

- Added a NEWS.md file.
- Ensure package passes `devtools::check()`

# msgr 1.0.2

- Made `is_na()` vectorised

# msgr 1.0.1

- Removed pipe operator so package does not require dependency
- Set `simplify = FALSE` in `sapply()` & `mapply()` and unlist result instead

# msgr 1.0.0

- Added pkgdown configuration

# msgr 0.11

- Added `try_pmap()`
- Added `try_map()`
- Added `try_catch()`

# msgr 0.10

- Ignore `R/on-load.R` in test coverage report
- Fix message in `assert()` (and add test for `*_if()` functions)
- Set options in `.onLoad()` not `.onAttach()`
- Check calling function before using it in messages
- Use `error()` when checking inputs instead of stop
- Removed doc links to base
- Added `is_function()` predicate

# msgr 0.9

- Added `assert()`
- Added `error_if()`
- Added `warn_if()`
- Added `info_if()`

# msgr 0.8

- Added `error()`
- Added `warn()`
- Added `info()`
- Add Rcheck file to buildignore config
- Removed unlinking of temp directory

# msgr 0.7

- Added `has_names()`
- Added `is_in()`

# msgr 0.6

- Added code coverage to Travis and README
- Add codecov configuration

# msgr 0.5

- Added `is_writeable()`
- Added `is_readable()`
- Added `is_file()`
- Added `is_dir()`
- Added `is_url()`
- Remove period from DESCRIPTION title

# msgr 0.4

- Added Travis config

# msgr 0.3

- Added `is_na()`
- Added `is_null()`
- Added `is_data_frame()`
- Added `is_list()`
- Added `is_string()`
- Added `is_character()`
- Added `is_natural()`
- Added `is_integer()`
- Added `is_number()`
- Added `is_numeric()`
- Added `is_boolean()`
- Added `is_logical()`
- Added `is_scalar()`
- Added `is_vector()`

# msgr 0.2

- Built package documentation
- Added `.onAttach()` function to validate options
- Added `.onLoad()` function to set environment variables and options

# msgr 0.1

- Edited DESCRIPTION file
- Set github links
- Set up package documentation
- Set up testthat structure
- Use markdown documentation
- Added MIT Licence
- Created package structure
- Updated README with brief description of package intent
