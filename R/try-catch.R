#  FUNCTION: try_catch ------------------------------------------------------------------------
#
#' Try to evaluate an expressions and capture any messages, warnings or errors
#'
#' This function is similar to [tryCatch()], except that, by default, errors are captured
#' and presented using [error()]. Messages and warnings are not captured by this function.
#' In addition, a "finally" expression can be specified which is evaluated at the end of
#' the call no matter the result.
#'
#' @param expr (expression) The expression to evaluate
#' @param on_error (function, optional) A function describing what to do in the event of a
#'   error in the above expression. The function must take a single argument, which is the
#'   [simpleError()]. If missing or `NULL`, errors are not caught. Default: [error()]
#'   called with the error's message prefixed by the calling function name.
#' @param finally (expression, optional) An expression to evaluate at the end of the call.
#'    If missing or `NULL`, nothing is actioned.
#'
#' @return The result of the evaluated expression
#'
#' @export
#'
try_catch <- function(
  expr,
  on_error,
  finally)
{
  if (missing(on_error) || is_null(on_error)) {
    if (sys.nframe() > 1) {
      calling_function <- deparse(sys.calls()[[sys.nframe() - 1]][[1]])
      prefix <- paste0("In ", calling_function, "(): ")
    } else {
      prefix <- ""
    }

    on_error = function(e) {
      if (grepl("^\\[[0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\] ", e$message)) {
        e$message <- sub("^\\[[0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\] ", "", e$message)
      }
      error(prefix, e$message)
    }
  }

  assert(is_function(on_error), "'on_error' must be a function")

  if (missing(finally)) {
    finally <- NULL
  }

  tryCatch(expr, error = on_error, finally = finally)
}

#  FUNCTION: try_map --------------------------------------------------------------------------
#
#' Apply a function over a vector or list, capturing any errors to display at the end
#'
#' This function is similar to [sapply()] except that instead of stopping at the first
#' error it captures them and continues. If there are any errors it collects them together
#' and displays them at the end. You have the option to specify a prefix to the error message
#' using the `msg_prefix` argument.
#'
#' If the mapped function is a long running process `try_map` can output a warning at the time
#' an error occurs, but specifying the `warn_level` argument to be greater than 0 (see
#' [warn()] for more details about message levels. Similarly `error_level` argument specifies
#' the level of any reported error, as detailed in [error()].
#'
#' If you do not want the function to stop with an error, you can insted return a warning or
#' info message using the `on_error` argument.
#'
#' Finally, `simplify` and `use_names` are the same as their [sapply()] conterparts, allowing
#' the user to specify whether to simplify the output to a vector, if possible, and whether
#' to use the vector input `x` as names to the resulting list.
#'
#' @param x (vector or list) The vector or list to map the function to.
#' @param f (function) The function to map to the elements of `x`.
#' @param ... (optional) Extra arguments to supply to f.
#' @param msg_prefix (string, optional) A message to prefix any resulting error message.
#' @param warn_level (integer, optional) The level of any warnings about errors encountered.
#'   If 0 then no warnings are shown. Default: 2.
#' @param error_level (integer, optional) The level of any resulting errors. Default: 1.
#' @param on_error (string) The kind of message to produce if there is an error. Either "info",
#'   "warn", or "error". Default: "error".
#' @param simplify (boolean, optional) Whether to try to simplify the result of the mapping
#'   (see [sapply()] for details). Default: FALSE.
#' @param use_names (boolean, optional) Whether to use 'x' as names in the resulting list. 'x'
#'   must be a character vector for this to work. Default: TRUE.
#'
#' @return If `simplify = FALSE` a list is returned. Otherwise, the function attempts to
#'   simplify the result to a vector or array.
#'
#' @export
#'
try_map <- function(
  x,
  f,
  ...,
  msg_prefix,
  warn_level  = 2,
  error_level = 1,
  on_error    = "error",
  simplify    = FALSE,
  use_names   = TRUE)
{
  {
    if (missing(msg_prefix)) {
      mapped_function <- as.character(substitute(f))
      if (sys.nframe() > 1) {
        calling_function <- deparse(sys.calls()[[sys.nframe() - 1]][[1]])
        msg_prefix <- paste0("In ", calling_function, "(): ")
      } else {
        msg_prefix <- "In try_map():"
      }
    }

    (is_vector(x) || is_list(x)) ||
      error("'x' must be an atomic vector or a list")
    (is_function(f)) ||
      error("'f' must be a function")
    (is_null(msg_prefix) || is_string(msg_prefix)) ||
      error("'msg_prefix' must be NULL or a string")
    (is_scalar(warn_level) && is_integer(warn_level) && warn_level >= 0) ||
      error("'warn_level' must be an integer greater or equal to 0")
    (is_natural(error_level)) ||
      error("'error_level' must be an integer greater than 0")
    (is_string(on_error) && on_error %in% c("info", "warn", "error")) ||
      error("'on_error' must be either 'info', 'warn' or 'error'")
    (is_boolean(simplify)) ||
      error("'simplify' must be boolean")
    (is_boolean(use_names)) ||
      error("'use_names' must be boolean")
  }

  result <- sapply(X = x, simplify = simplify, USE.NAMES = use_names, ..., FUN = function(x, ...) {
    tryCatch({
      f(x, ...)
    },
    error = function(e) {
      if (nchar(as.character(x)) > 20) {
        x <- paste0(substr(as.character(x), 1, 20), "...")
      }

      if (warn_level > 0) warn("Failed for ", formalArgs(f)[1], " = ", x, level = warn_level)
      if (is_string(mapped_function)) {
        e$message <- paste0("In ", mapped_function, "(): ", e$message)
      }
      e
    })
  })

  if (!use_names) {
    names(result) <- NULL
  }

  if (any(sapply(result, is, "error"))) {
    if (is.null(names(result))) {
      prefix <- "\n"
    } else {
      prefix <- paste0("\n'", names(result)[sapply(result, is, "error")], "': ")
    }

    error_messages <- result[sapply(result, is, "error")] %>% sapply(getElement, "message")
    error_msg <- paste0(prefix, error_messages, collapse = "\n")
    if (!is_null(msg_prefix)) {
      error_msg <- paste0(msg_prefix, "\n", error_msg)
    }

    switch(
      on_error,
      error = error(error_msg, level = error_level),
      warn  = warn( error_msg, level = error_level),
      info  = info( error_msg, level = error_level))
  }

  result
}
