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
#' @param from Name of the caller function.
#'
#' @return
#' Nothing in case of success, otherwise an error is thrown.
#'
#' @noRd
validate_class <- function(arg, classes, from = "fuzz") {
  name <- sprintf("'%s'", all.vars(match.call())[1])
  if (missing(arg) || sum(inherits(arg, classes)) == 0L) {
    fuzz_error(name, "should be of class", paste(classes, collapse = ", "),
               from = from)
  }
  if (length(arg) == 0) {
    fuzz_error(name, "is empty", from = from)
  }
}

#' @title Stop with an error message
#'
#' @param ... Strings that are joined together in the error message.
#' @param from Name of the caller function.
#'
#' @noRd
fuzz_error <- function(..., from = "fuzz") {
  stop(do.call(paste, c(sprintf("[%s]", from), list(...))), call. = FALSE)
}

#' @title Check that a function can be fuzzed
#'
#' @param fun Function to validate.
#' @param pkg Name of the package where functions are searched. A `NULL`
#'        value corresponds to the global namespace. This is used only to
#'        generate a better message.
#'
#' @return
#' In case of failure, a character string containing the reason why the
#' function cannot be fuzzed; otherwise the function itself.
#'
#' @noRd
check_fuzzable <- function(fun, pkg) {
  ## skip non-existing names
  if (inherits(fun, "try-error"))
    return(sprintf("Object not found in the %s namespace",
                   if (is.null(pkg)) "global" else sprintf("'%s'", pkg)))

  ## skip non-functions
  if (!is.function(fun))
    return("Not a function")

  ## skip functions accept no arguments
  if (suppressWarnings(length(formals(fun))) == 0 && !is.primitive(fun))
    return("Doesn't accept arguments")

  ## skip functions that wait for user input
  if (contains_readline(fun))
    return("Contains readline()")

  return(fun)
}

#' Generate coloured summary statistics from the results
#'
#' This computes summary statistics from the fuzzing results, prints a
#' message for the overall success or failure, and returns a summary string.
#'
#' @param object An object of class `cbtf`.
#'
#' @return
#' A summary results string formatted with ANSI colour codes.
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
  summary.stats <- paste(tocolour("FAIL", sum(stats$FAIL)),
                         tocolour("WARN", sum(stats$WARN)),
                         tocolour("SKIP", sum(stats$SKIP)),
                         tocolour("OK",   sum(stats$OK), success),
                         sep = " | ")
  paste("[", summary.stats, "]")
}

#' Add colour formatting to a string
#'
#' @param res A result string, one of "OK", "SKIP", "WARN" or "FAIL".
#' @param num A numerical value: if it evaluates to a positive finite value
#'        and `colour` is `TRUE`, then the string is coloured. The default
#'        value (`Inf`) implies that colour is applied, but `num` is not
#'        printed out.
#' @param colour A logical value that determines if the colour should be
#'        applied. If `FALSE`, nothing gets coloured independently of `num`.
#'
#' @return
#' A string formatted with ANSI colour codes.
#'
#' @noRd
tocolour <- function(res, num = Inf, colour = TRUE) {
  if (num > 0 && colour) {
    res <- list(FAIL = cli::col_yellow, WARN = cli::col_magenta,
                SKIP = cli::col_blue, OK = cli::col_green)[[res]](res)
  }
  paste0(res, if (!is.infinite(num)) sprintf(" %d", num))
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

#' Add names to an unnamed list of inputs
#'
#' @param what A list of inputs created with alist().
#'
#' @return
#' A named list with its elements evaluated. The names automatically generated
#' by deparsing the symbols in `what`.
#'
#' @noRd
add_names_to_alist <- function(what) {
  names(what) <- sapply(what, function(x) deparse(x))
  lapply(what, eval)
}
