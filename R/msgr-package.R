#  PACKAGE: msgr ------------------------------------------------------------------------------
#
#' Extends messages, warnings and errors by adding levels and log files
#'
#' Provides new functions [info()], [warn()] and [error()], similar to [message()],
#' [warning()] and [stop()] respectively. However, the new functions can have a `level`
#' associated with them, so that when executed the global level option determines whether
#' they are shown or not. This allows dobug modes, outputting more information. The can also
#' output all messages to a log file.
#'
#' @name msgr
#' @docType package
#'
#' @seealso
#' Useful links:
#' - \url{https://github.com/ChadGoymer/msgr}
#' - Report bugs at \url{https://github.com/ChadGoymer/msgr/issues}
#'
#' @author Chad Goymer \email{chad.goymer@@gmail.com}
#'
#' @importFrom rlang is_integerish is_scalar_integerish
#' @import purrr
#'
globalVariables(c(".", ".data"))
