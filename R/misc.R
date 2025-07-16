##===========================================================================
##
## Copyright (c) 2025 Marco Colombo
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.
##
##===========================================================================

#' @title Validate that an argument is of the specified class
#'
#' @param arg Argument to validate.
#' @param classes A vector of candidate classes or types.
#'
#' @return
#' Nothing in case of success, otherwise an error is thrown.
#'
#' @noRd
validate_class <- function(arg, classes) {
  if (missing(arg) || sum(inherits(arg, classes)) == 0L) {
    name <- sprintf("'%s'", all.vars(match.call())[1])
    stop(paste0(name, " should be of class ",
                paste(classes, collapse = ", ")), call. = FALSE)
  }
}

#' @title Validate that an argument is not missing
#'
#' @param arg Argument to validate.
#'
#' @return
#' Nothing in case of success, otherwise an error is thrown.
#'
#' @noRd
validate_not_missing <- function(arg) {
  if (missing(arg)) {
    name <- sprintf("'%s'", all.vars(match.call())[1])
    stop(paste0(name, " must be specified"), call. = FALSE)
  }
}

#' @title Validate that a function can be fuzzed
#'
#' @param fun Function to validate.
#'
#' @return
#' In case of failure, a character string containing the reason why the
#' function cannot be fuzzed; otherwise the function itself.
#'
#' @noRd
validate_fuzzable <- function(fun) {
  ## skip non-existing names
  if (inherits(fun, "try-error"))
    return("Object not found")

  ## skip non-functions
  if (!is.function(fun))
    return("Not a function")

  ## skip functions accept no arguments
  if (suppressWarnings(length(formals(fun))) == 0)
    return("Doesn't accept arguments")

  ## skip functions that wait for user input
  if (contains_readline(fun))
    return("Contains readline()")

  return(fun)
}

#' Generate summary statistics from the results
#'
#' This computes summary statistics from the fuzzing results, prints a
#' message for the overall success or failure, and returns a summary string.
#'
#' @param object An object of class `cbtf`.
#'
#' @return
#' A summary results string.
#'
#' @noRd
compute_summary_stats <- function(object) {
  results <- unlist(lapply(object$runs, function(x) x$res))
  success <- sum(results %in% c("FAIL", "WARN")) == 0
  if (success)
    cli::cli_alert_success(" \U0001F3C3 You didn't get caught by the fuzz!")
  else
    cli::cli_alert_danger(" \U0001F6A8   CAUGHT BY THE FUZZ!   \U0001F6A8")

  stats <- as.list(table(results))
  summary.stats <- paste("FAIL", sum(stats$FAIL), " | ",
                         "WARN", sum(stats$WARN), " | ",
                         "SKIP", sum(stats$SKIP), " | ",
                         "OK",   sum(stats$OK))
  paste("[", summary.stats, "]")
}

#' Check if the body of a function contains calls to readline()
#'
#' @param fun An expression.
#'
#' @return
#' A logical value.
#'
#' @noRd
contains_readline <- function(expr) {
  if (is.function(expr))
    expr <- body(expr)
  any(sapply(expr, function(line) {
    if (length(line) > 1)
      return(contains_readline(line))
    any(grepl("^readline", deparse(line)))
  }))
}
