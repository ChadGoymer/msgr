# msgr 1.0.3

- Added a NEWS.md file
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
- Added `try_map`
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
- Added `onAttach()` function to validate options
- Added `onLoad()` function to set environment variables and options

# msgr 0.1

- Edited DESCRIPTION file
- Set github links
- Set up package documentation
- Set up testthat structure
- Use markdown documentation
- Added MIT Licence
- Created package structure
- Updated README with brief description of package intent
