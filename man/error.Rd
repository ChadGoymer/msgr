% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/messages.R
\name{error}
\alias{error}
\title{Display an error, and record it in a log file.}
\usage{
error(
  ...,
  level = 1,
  msg_level = getOption("msgr.level"),
  msg_types = getOption("msgr.types"),
  log_path = getOption("msgr.log_path")
)
}
\arguments{
\item{...}{(strings) error to be displayed or written to file.}

\item{level}{(integer, optional) The level of the error, from 1 to 10.
Default: 1.}

\item{msg_level}{(integer, optional) The maximum level of messages to output.
Default: set in the option \code{"msgr.level"}.}

\item{msg_types}{(character, optional) The type to write or display. Must
either NULL or one or more from "INFO", "WARNING" or "ERROR". Default: set
in the option \code{"msgr.types"}.}

\item{log_path}{(string, optional) The file path to the text log file. If set
to "", then no logs are written. Default: set in the option
\code{"msgr.log_path"}.}
}
\value{
A string is return invisibly containing the error
}
\description{
\code{error()} is similar to \code{\link[=stop]{stop()}}, but it also writes the error to a log file.
Whether it is shown, or written to the log, depends on the level and type of
the error. See details below for more information.
}
\details{
Whether an error is shown, or written to the log, depends on two options:
\enumerate{
\item \strong{Level}: This allows control over the depth of messages. Each message
can be assigned a \code{level} and if it is below the \code{msg_level} (set in the
package option \code{msgr.level} by default) the message is displayed and
written to the log.
\item \strong{Type}: The type is refers to whether the message is "INFO", "WARNING"
or "ERROR", as produced by the functions \code{\link[=info]{info()}}, \code{\link[=warn]{warn()}} and \code{\link[=error]{error()}}
respectively. If the message type is in the \code{msg_types} (set in the
package option \code{msgr.types} by default) the message is displayed and
written to the log. This allows you to for instance, just display errors
and warnings and ignore messages.
}

The location of the log file is set in the package option \code{msgr.log_path}, or
as an argument to this function. messages are added with a time stamp. If the
\code{log_path} is equal to "" then no log is produced.
}
\examples{
\dontrun{

  # Use error() to create timed errors
  error("This is a simple error")
  error("This is a level 2 error, so not shown by default", level = 2)

  # Set default level in options to determine what is shown
  options(msgr.level = 2)
  error("This is a level 2 error, so is shown now", level = 2)

}

}
