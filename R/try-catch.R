#  FUNCTION: try_catch ---------------------------------------------------------
#
#' Try to evaluate an expressions and capture any messages, warnings or errors
#'
#' `try_catch()` is similar to [tryCatch()], except that, by default, errors are
#' captured and presented using [error()]. Messages and warnings are not
#' captured by this function. In addition, a "finally" expression can be
#' specified which is evaluated at the end of the call no matter the result.
#'
#' `try_silenty()` is similar to `try(..., silently = TRUE)` however if there is
#' an error it returns `NULL`. All messages and warnings are also suppressed.
#'
#' @param expr (expression) The expression to evaluate
#' @param on_error (function, optional) A function describing what to do in the
#'   event of a error in the above expression. The function must take a single
#'   argument, which is the [simpleError()]. If missing or `NULL`, errors are
#'   not caught. Default: [error()] called with the error's message prefixed by
#'   the calling function name.
#' @param finally (expression, optional) An expression to evaluate at the end of
#'   the call. If missing or `NULL`, nothing is actioned.
#'
#' @return The result of the evaluated expression
#'
#' @examples \dontrun{
#'
#' try_catch(x <- "foo")
#' try_catch(stop("This is an error"))
#'
#' try_silently("This is fine")
#' try_silently(stop("This is an error"))
#'
#' }
#'
#' @export
#'
try_catch <- function(
  expr,
  on_error,
  finally = NULL
) {
  if (missing(on_error) || is.null(on_error)) {
    if (sys.nframe() > 1) {
      calling_function <- deparse(sys.calls()[[sys.nframe() - 1]][[1]])
      prefix <- paste0("In ", calling_function, "(): ")
    } else {
      prefix <- ""
    }

    on_error <- function(e) {
      error(prefix, remove_time(e$message))
    }
  }

  assert(is.function(on_error), "'on_error' must be a function")
  tryCatch(expr, error = on_error, finally = finally)
}

#  FUNCTION: try_silently ------------------------------------------------------
#
#' @rdname try_catch
#' @export
#'
try_silently <- function(
  expr
) {
  tryCatch({
    sink(fs::file_temp())
    on.exit(sink())
    invisible(suppressWarnings(suppressMessages(force(expr))))
  }, error = function(e) NULL)
}

#  FUNCTION: with_msgr ---------------------------------------------------------
#
#' Convert messages, warnings and stops into msgr messages
#'
#' This function allows you to execute an expression and all messages, warnings
#' and stops are converted to [msgr::info()], [msgr::warn()] and [msgr::error()]
#' respectively.
#'
#' This function can also be used to change the `level` of message, so if a
#' function raises a message with level 1, it can be downgraded to level 2+.
#' Similarly it can also be used to modify the `msg_level`. For example, if the
#' current `msg_level` is set to 1, and a function raises a message with level
#' 3, the message will not be displayed. If you want it displayed for this
#' function but not everywhere else, you can use `with_msgr()` and set the
#' `msg_level` to 3 for that call alone.
#'
#' @param expr (expression) The expression to execute.
#' @param level (integer, optional) The level of messages, from 1 to 10.
#'   Default: 1.
#' @param warn_level (integer, optional) The level of warnings, from 1 to 10.
#'   Default: 1.
#' @param error_level (integer, optional) The level of errors, from 1 to 10.
#'   Default: 1.
#' @param msg_level (integer, optional) The maximum level of messages to output.
#'   Default: set in the option `"msgr.level"`.
#' @param msg_types (character, optional) The type to write or display. Must
#'   either NULL or one or more from "INFO", "WARNING" or "ERROR". Default: set
#'   in the option `"msgr.types"`.
#' @param log_path (string, optional) The file path to the text log file. If set
#'   to "", then no logs are written. Default: set in the option
#'   `"msgr.log_path"`.
#'
#' @return Whatever the expression returns
#'
#' @examples \dontrun{
#'
#' test_msgr <- function(x) {
#'   if (x == "msg") message("This is a message")
#'   if (x == "wrn") warning("This is a warning")
#'   if (x == "err") stop("This is an error")
#' }
#'
#' with_msgr(test_msgr("msg"))
#' with_msgr(test_msgr("wrn"))
#' with_msgr(test_msgr("err"))
#'
#' with_msgr(test_msgr("msg"), level = 2)
#' with_msgr(test_msgr("wrn"), level = 2)
#' with_msgr(test_msgr("err"), level = 2)
#'
#' test_lvl <- function(x) {
#'   if (x == "msg") info("This is a message", level = 3)
#'   if (x == "wrn") warn("This is a warning", level = 3)
#'   if (x == "err") error("This is an error", level = 3)
#' }
#'
#' with_msgr(test_lvl("msg"), msg_level = 3)
#' with_msgr(test_lvl("wrn"), msg_level = 3)
#' with_msgr(test_lvl("err"), msg_level = 3)
#'
#' }
#'
#' @export
#'
with_msgr <- function(
  expr,
  level       = 1,
  warn_level  = level,
  error_level = 1,
  msg_level   = getOption("msgr.level"),
  msg_types   = getOption("msgr.types"),
  log_path    = getOption("msgr.log_path")
) {
  is_natural(level, n = 1) && is_in_range(level, min = 1, max = 10) ||
    stop("'level' must be an integer between 1 and 10: ", level)
  is_natural(warn_level, n = 1) && is_in_range(warn_level, min = 1, max = 10) ||
    stop("'warn_level' must be an integer between 1 and 10: ", warn_level)
  is_natural(error_level, n = 1) &&
    is_in_range(error_level, min = 1, max = 10) ||
    stop("'error_level' must be an integer between 1 and 10: ", error_level)
  is_natural(msg_level, n = 1) && is_in_range(msg_level, min = 1, max = 10) ||
    stop("'msg_level' must be an integer between 1 and 10: ", msg_level)
  is.null(msg_types) || is.character(msg_types) ||
    stop("'msg_types' must be NULL or a character vector: ", msg_types)
  all(is_in(msg_types, c("INFO", "WARNING", "ERROR"))) ||
    stop("'msg_types' must be either 'INFO', 'WARNING' or 'ERROR': ", msg_types)
  is.character(log_path) && length(log_path) == 1 ||
    stop("'log_path' must be a string: ", log_path)

  original_options <- options(
    msgr.level    = msg_level,
    msgr.types    = msg_types,
    msgr.log_path = log_path
  )
  on.exit(options(original_options))

  withCallingHandlers(
    withRestarts(
      expr,
      muffleMessage = function() NULL,
      muffleWarning = function() NULL,
      muffleStop    = function() NULL
    ),
    message = function(m) {
      msgr::info(remove_time(m$message), level = level)
      invokeRestart("muffleMessage")
    },
    warning = function(w) {
      msgr::warn(remove_time(w$message), level = warn_level)
      invokeRestart("muffleWarning")
    },
    error   = function(e) {
      msgr::error(remove_time(e$message), level = error_level)
      invokeRestart("muffleWarning")
    }
  )
}

#  FUNCTION: try_map -----------------------------------------------------------
#
#' Apply a function over a vector or list, capturing any errors to display at
#' the end
#'
#' This function is similar to [purrr::map()] except that instead of stopping at
#' the first error it captures them and continues. If there are any errors it
#' collects them together and displays them at the end. You have the option to
#' specify a prefix to the error message using the `msg_prefix` argument.
#'
#' If the mapped function is a long running process `try_map()` can output a
#' warning at the time an error occurs, but specifying the `warn_level` argument
#' to be greater than 0 (see [warn()] for more details about message levels.
#' Similarly `error_level` argument specifies the level of any reported error,
#' as detailed in [error()].
#'
#' If you do not want the function to stop with an error, you can instead return
#' a warning or info message using the `on_error` argument.
#'
#' Finally, `simplify` and `use_names` allow the user to specify whether to
#' simplify the output to an atomic vector, if possible, and whether to use the
#' vector input `x` as names to the resulting list.
#'
#' @param x (vector or list) The vector or list to map the function to.
#' @param f (function) The function to map to the elements of `x`.
#' @param ... (optional) Extra arguments to supply to f.
#' @param msg_prefix (string, optional) A message to prefix any resulting error
#'   message.
#' @param warn_level (integer, optional) The level of any warnings about errors
#'   encountered. If 0 then no warnings are shown. Default: 2.
#' @param error_level (integer, optional) The level of any resulting errors.
#'   Default: 1.
#' @param on_error (string) The kind of message to produce if there is an error.
#'   Either "info", "warn", or "error". Default: "error".
#' @param simplify (boolean, optional) Whether to try to simplify the result of
#'   the mapping into an atomic vector. Default: FALSE.
#' @param use_names (boolean, optional) Whether to use 'x' as names in the
#'   resulting list. 'x' must be a character vector for this to work. Default:
#'   TRUE.
#'
#' @return If `simplify = FALSE` a list is returned. Otherwise, the function
#'   attempts to simplify the result to an atomic vector or array.
#'
#' @examples \dontrun{
#'
#'   test_try_map <- function(x, y) if (x > y) stop("x > y") else x
#'   try_map(1:3, test_try_map, y = 2)
#'   try_map(1:3, test_try_map, y = 5)
#'
#' }
#'
#' @export
#'
try_map <- function(
  x,
  f,
  ...,
  msg_prefix  = NULL,
  warn_level  = 2,
  error_level = 1,
  on_error    = "error",
  simplify    = FALSE,
  use_names   = TRUE
) {
  if (is.null(msg_prefix)) {
    mapped_function <- as.character(substitute(f))
    if (sys.nframe() > 1) {
      calling_function <- deparse(sys.calls()[[sys.nframe() - 1]][[1]])
      msg_prefix <- paste0("In ", calling_function, "(): ")
    } else {
      msg_prefix <- "In try_map():"
    }
  }

  is.vector(x) ||
    error("'x' must be a vector")
  is.function(f) ||
    error("'f' must be a function")
  is.character(msg_prefix) && length(msg_prefix) == 1 ||
    error("'msg_prefix' must be a character vector of length 1")
  is_natural(warn_level, n = 1) ||
    error("'warn_level' must be a positive integer vector of length 1")
  is_natural(error_level, n = 1) ||
    error("'error_level' must be a positive integer vector of length 1")
  is.character(on_error) && length(on_error) == 1 &&
    is_in(on_error, c("info", "warn", "error")) ||
    error("'on_error' must be either 'info', 'warn' or 'error'")
  is.logical(simplify) && length(simplify) == 1 ||
    error("'simplify' must be a logical vector of length 1")
  is.logical(use_names) && length(use_names) == 1 ||
    error("'use_names' must be a logical vector of length 1")

  result <- purrr::map(.x = x, ..., .f = function(x, ...) {
    tryCatch({
      f(x, ...)
    },
    error = function(e) {
      if (nchar(as.character(x)) > 20) {
        x <- paste0(substr(as.character(x), 1, 20), "...")
      }

      if (warn_level > 0) {
        warn("Failed for ", names(formals(f))[1], " = ", x, level = warn_level)
      }

      if (is.character(mapped_function) && length(mapped_function) == 1) {
        e$message <- paste0("In ", mapped_function, "(): ", e$message)
      }

      e
    })
  })

  if (use_names && is.character(x)) {
    names(result) <- x
  }

  is_error <- purrr::map_lgl(result, function(r) "error" %in% class(r))

  if (any(is_error)) {
    prefix <- ""
    if (!is.null(names(result))) {
      prefix <- paste0("'", names(result)[is_error], "': ")
    }

    error_msg <- paste0(
      prefix, purrr::map_chr(result[is_error], "message"),
      collapse = "\n"
    )
    if (!is.null(msg_prefix)) {
      error_msg <- paste0(msg_prefix, "\n", error_msg)
    }

    switch(
      on_error,
      error = error(error_msg, level = error_level),
      warn  = warn(error_msg,  level = error_level),
      info  = info(error_msg,  level = error_level)
    )
  }

  if (simplify && all(purrr::map_int(result, length) == 1L)) {
    result <- unlist(result)
  }

  result
}

#  FUNCTION: try_pmap ----------------------------------------------------------
#
#' Apply a function over a list of vectors, capturing any errors to display at
#' the end
#'
#' This function is similar to [purrr::pmap()] except that instead of stopping
#' at the first error it captures them and continues. If there are any errors it
#' collects them together and displays them at the end. You have the option to
#' specify a prefix to the error message using the `msg_prefix` argument.
#'
#' If the mapped function is a long running process `try_pmap` can output a
#' warning at the time an error occurs, but specifying the `warn_level` argument
#' to be greater than 0 (see [warn()] for more details about message levels.
#' Similarly `error_level` argument specifies the level of any reported error,
#' as detailed in [error()].
#'
#' If you do not want the function to stop with an error, you can instead return
#' a warning or info message using the `on_error` argument.
#'
#' Finally, `simplify` and `use_names` allow the user to specify whether to
#' simplify the output to an atomic vector, if possible, and whether to use the
#' vector input `x` as names to the resulting list.
#'
#' @param l (list) A list of vectors the same length to apply the function to.
#' @param f (function) The function to map to the elements of the vectors in
#'   `l`.
#' @param ... (optional) Extra arguments to supply to f.
#' @param msg_prefix (string, optional) A message to prefix any resulting error
#'   message.
#' @param warn_level (integer, optional) The level of any warnings about errors
#'   encountered. If 0 then no warnings are shown. Default: 2.
#' @param error_level (integer, optional) The level of any resulting errors.
#'   Default: 1.
#' @param on_error (string) The kind of message to produce if there is an error.
#'   Either "info", "warn", or "error". Default: "error".
#' @param simplify (boolean, optional) Whether to try to simplify the result of
#'   the mapping into an atomic vector. Default: FALSE.
#' @param use_names (boolean, optional) Whether to use 'x' as names in the
#'   resulting list. 'x' must be a character vector for this to work. Default:
#'   TRUE.
#'
#' @return If `simplify = FALSE` a list is returned. Otherwise, the function
#'   attempts to simplify the result to an atomic vector.
#'
#' @examples \dontrun{
#'
#'   test_try_pmap <- function(x, y) if (x > y) stop("x > y") else x
#'   try_pmap(list(1:3, 3:1), test_try_pmap)
#'   try_pmap(list(1:3, 2:4), test_try_pmap)
#'
#' }
#'
#' @export
#'
try_pmap <- function(
  l,
  f,
  ...,
  msg_prefix  = NULL,
  warn_level  = 2,
  error_level = 1,
  on_error    = "error",
  simplify    = FALSE,
  use_names   = TRUE
) {
  if (is.null(msg_prefix)) {
    mapped_function <- as.character(substitute(f))
    if (sys.nframe() > 1) {
      calling_function <- deparse(sys.calls()[[sys.nframe() - 1]][[1]])
      msg_prefix <- paste0("In ", calling_function, "(): ")
    } else {
      msg_prefix <- "In try_pmap():"
    }
  }

  is.list(l) && length(unique(purrr::map_int(l, length)) == 1) ||
    error("'l' must be a list of vectors with equal length")
  is.function(f) ||
    error("'f' must be a function")
  is.character(msg_prefix) && length(msg_prefix) == 1 ||
    error("'msg_prefix' must be a character vector of length 1")
  is_natural(warn_level, n = 1) ||
    error("'warn_level' must be a positive integer vector of length 1")
  is_natural(error_level, n = 1) ||
    error("'error_level' must be a positive integer vector of length 1")
  is.character(on_error) && length(on_error) == 1 &&
    is_in(on_error, c("info", "warn", "error")) ||
    error("'on_error' must be either 'info', 'warn' or 'error'")
  is.logical(simplify) && length(simplify) == 1 ||
    error("'simplify' must be a logical vector of length 1")
  is.logical(use_names) && length(use_names) == 1 ||
    error("'use_names' must be a logical vector of length 1")

  result <- purrr::pmap(.l = l, ..., .f = function(...) {
    tryCatch({
      rlang::exec(f, ...)
    },
    error = function(e) {
      args <- list(...)

      if (nchar(as.character(args[[1]])) > 20) {
        args[[1]] <- paste0(substr(as.character(args[[1]]), 1, 20), "...")
      }

      if (warn_level > 0) {
        warn(
          "Failed for ", names(formals(f))[1], " = ", args[[1]],
          level = warn_level
        )
      }

      if (is.character(mapped_function) && length(mapped_function) == 1) {
        e$message <- paste0("In ", mapped_function, "(): ", e$message)
      }

      e
    })
  })

  if (use_names && is.character(l[[1]])) {
    names(result) <- l[[1]]
  }

  is_error <- purrr::map_lgl(result, function(r) "error" %in% class(r))

  if (any(is_error)) {
    prefix <- ""
    if (!is.null(names(result))) {
      prefix <- paste0("'", names(result)[is_error], "': ")
    }

    error_msg <- paste0(
      prefix, purrr::map_chr(result[is_error], "message"),
      collapse = "\n"
    )
    if (!is.null(msg_prefix)) {
      error_msg <- paste0(msg_prefix, "\n", error_msg)
    }

    switch(
      on_error,
      error = error(error_msg, level = error_level),
      warn  = warn(error_msg,  level = error_level),
      info  = info(error_msg,  level = error_level)
    )
  }

  if (simplify && all(purrr::map_int(result, length) == 1L)) {
    result <- unlist(result)
  }

  result
}

#  FUNCTION: remove_time -------------------------------------------------------
#
#' Remove time component from a message
#'
#' @param msg (character) The message to remove time from
#'
#' @return The message with the time removed
#'
#' @noRd
#'
remove_time <- function(msg) {
  if (grepl("^\\[[0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\] ", msg)) {
    msg <- sub(
      pattern     = "^\\[[0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\] ",
      replacement = "",
      x           = msg
    )
  }
  msg
}
