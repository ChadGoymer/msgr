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
