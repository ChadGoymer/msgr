# msgr

[![Build status](https://travis-ci.org/ChadGoymer/msgr.svg?branch=master)](https://travis-ci.org/ChadGoymer/msgr) [![Code coverage](https://codecov.io/github/ChadGoymer/msgr/branch/master/graphs/badge.svg)](https://codecov.io/github/ChadGoymer/msgr) 

This package extends the `message()`, `warning()` and `stop()` functions by adding _levels_ 
of messages and the option to record them in a log file.

## Types

Messages come in three _types_: "INFO", "WARNING" and "ERROR", as produced by the functions
`info()`, `warn()` and `error()`, which are equivalent to `message()`, `warning()` and 
`stop()`. respectively. When executing code the _types_ to display can be specified as an 
option. For example, you can ignore "INFO" and only show "WARNING"s and "ERROR"s by setting:

```r
options(msgr.types = c("WARNING", "ERROR"))
```

## Levels

Whenever `info()`, `warn()` or `error()` are used a level can be specified, and when 
executing the code the _levels_ to display can be specified as an option. The message is
only shown if its level is less than, or equal to, the _level_ option.

```r
options(msgr.level = 1)

warn("This is an important warning!", level = 1)

info("This is useful information, but not important!", level = 3)
```

## Log file

Message can also be written to a log file. The log file can be specified each time you use
`info()`, `warn()` or `error()`, or you can specify it as an option.

```r
options(msgr.log_path = "~/msgr.log")

info("This is written to console and log file")

error("This is written to console and an error log", log_path = "~/msgr-errors.log")
```
