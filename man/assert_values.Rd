% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assert.R
\name{assert_na}
\alias{assert_na}
\alias{assert_in}
\alias{assert_in_range}
\alias{assert_char_length}
\title{Display an error if not the allowed values}
\usage{
assert_na(
  x,
  level = 1,
  msg_level = getOption("msgr.level"),
  msg_types = getOption("msgr.types"),
  log_path = getOption("msgr.log_path")
)

assert_in(
  x,
  values,
  level = 1,
  msg_level = getOption("msgr.level"),
  msg_types = getOption("msgr.types"),
  log_path = getOption("msgr.log_path")
)

assert_in_range(
  x,
  min = NULL,
  max = NULL,
  level = 1,
  msg_level = getOption("msgr.level"),
  msg_types = getOption("msgr.types"),
  log_path = getOption("msgr.log_path")
)

assert_char_length(
  x,
  n = NULL,
  n_min = NULL,
  n_max = NULL,
  level = 1,
  msg_level = getOption("msgr.level"),
  msg_types = getOption("msgr.types"),
  log_path = getOption("msgr.log_path")
)
}
\arguments{
\item{x}{(any) The variable to check.}

\item{level}{(integer, optional) The level of the message, from 1 to 10.
Default: 1.}

\item{msg_level}{(integer, optional) The maximum level of messages to output.
Default: set in the option \code{"msgr.level"}.}

\item{msg_types}{(character, optional) The type to write or display. Must
either NULL or one or more from "INFO", "WARNING" or "ERROR". Default: set
in the option \code{"msgr.types"}.}

\item{log_path}{(character, optional) The file path to the text log file. If
set to "", then no logs are written. Default: set in the option
\code{"msgr.log_path"}.}

\item{values}{(character) The allowed values.}

\item{min}{(numeric, optional) The minimum allowed value for a range. If
\code{NULL} the minimum value is not checked. Default: \code{NULL}.}

\item{max}{(numeric, optional) The maximum allowed value for a range. If
\code{NULL} the maximum value is not checked. Default: \code{NULL}.}

\item{n}{(integer, optional) The allowed length. If \code{NULL} the exact
character length is not checked. Default: \code{NULL}.}

\item{n_min}{(integer, optional) The allowed minimum length. If \code{NULL} the
minimum character length is not checked. Default: \code{NULL}.}

\item{n_max}{(integer, optional) The allowed maximum length. If \code{NULL} the
maximum character length is not checked. Default: \code{NULL}.}
}
\value{
If assertion passes then \code{TRUE} is returned. This allows you to make
multiple assertions separated by \code{&}.
}
\description{
This function calls the \code{\link[=error]{error()}} function to display an error if the
variable's values are not valid.
}
\details{
The following validations can be checked:

The following values can be checked:
\itemize{
\item \code{assert_na()}: All elements of 'x' must be \code{NA}.
\item \code{assert_in()}: All elements of 'x' must in specified \code{values}.
\item \code{assert_in_range()}: All elements of 'x' must be in the specified numeric
range.
\item \code{assert_char_length}: All elements of 'x' must have valid character length.
You can specify the exact length using \code{n} or the minimum and/or maximum
length using \code{n_min} and \code{n_max} respectively.
}
}
\examples{
\dontrun{

# No error
assert_na(NA)
assert_na(c(NA, NA, NA))
# Error
assert_na(1)
assert_na(c(1, NA, 3))

# No error
assert_in(1:3, 1:10)
assert_in(c("A", "B"), LETTERS)
# Error
assert_in(8:12, 1:10)
assert_in(c("A", "B"), letters)

# No error
assert_in_range(1:3, min = 1)
assert_in_range(1:3, min = 1, max = 10)
# Error
assert_in_range(1:3, min = 5)
assert_in_range(1:3, min = 1, max = 2)

# Error
assert_char_length(c("bob", "jan"), n = 3)
assert_char_length(c("bob", "jan"), n_min = 1)
assert_char_length(c("bob", "jan"), n_min = 1, n_max = 10)
# No error
assert_char_length(c("bob", "jane"), n = 3)
assert_char_length(c("bob", "jane"), n_min = 5)
assert_char_length(c("bob", "jane"), n_min = 1, n_max = 2)

}

}
