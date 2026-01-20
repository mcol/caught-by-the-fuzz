##===========================================================================
##
## Copyright (c) 2025-2026 Marco Colombo
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
#' @param class A character string for the candidate class or type.
#' @param null.ok Whether a `NULL` value should be considered valid (`FALSE`
#'        by default).
#' @param from Name of the caller function.
#' @param scalar Whether to consider the argument valid only if it's a scalar
#'        value (`FALSE` by default).
#' @param min Minimum value considered value, or `NULL`.
#' @param remove_empty Discard empty elements before checking that `arg` is
#'        empty (`FALSE` by default).
#'
#' @return
#' Nothing in case of success, otherwise an error is thrown.
#'
#' @noRd
validate_class <- function(arg, class, null.ok = FALSE, from = "fuzz",
                           scalar = FALSE, min = NULL, remove_empty = FALSE) {
  !missing(arg) && is.null(arg) && null.ok && return()
  name <- sprintf("'%s'", all.vars(match.call())[1])
  if (missing(arg) || sum(inherits(arg, class)) == 0L ||
      (!is.list(arg) && length(arg) == 1 && (is.na(arg) || is.infinite(arg)))) {
    fuzz_error(name, "should be of class", paste(class, collapse = ", "),
               from = from)
  }
  scalar && length(arg) > 1 &&
    fuzz_error(name, "should be a", class(arg), "scalar", from = from)
  if (!is.null(min) && (length(arg) == 0 || arg < min))
    fuzz_error(name, "should be at least", min, from = from)
  if (remove_empty)
    arg <- arg[nchar(arg) > 0]
  length(arg) == 0 &&
    fuzz_error(name, "is an empty", class(arg), from = from)
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
#' @param fun Name of the function to validate.
#' @param pkg Name of the package where functions are searched. A `NULL`
#'        value corresponds to the global namespace.
#' @param ignore_deprecated Whether deprecated functions should be ignored
#'        (`TRUE` by default).
#' @param num_args Number of arguments being fuzzed (1 by default).
#'
#' @return
#' In case of failure, a character string containing the reason why the
#' function cannot be fuzzed; otherwise the function itself.
#'
#' @noRd
check_fuzzable <- function(fun, pkg, ignore_deprecated = TRUE, num_args = 1L) {
  ## attempt to get a function from its name
  fun <- try(if (is.null(pkg)) get(fun)
             else utils::getFromNamespace(fun, pkg),
             silent = TRUE)

  ## skip non-existing names
  inherits(fun, "try-error") &&
    return(sprintf("Object not found in the %s namespace",
                   if (is.null(pkg)) "global" else sprintf("'%s'", pkg)))

  ## skip non-functions
  is.function(fun) ||
    return("Not a function")

  ## skip primitive functions that don't report arguments
  is.null(args(fun)) &&
    return("Doesn't specify number of arguments")

  ## skip functions accepting no arguments
  formal.args <- formals(args(fun))
  num.formals <- length(formal.args)
  num.formals == 0L &&
    return("Doesn't accept arguments")

  ## adjust the number of arguments
  if (any(grepl("...", names(formal.args), fixed = TRUE)))
    num.formals <- Inf

  ## skip functions accepting fewer arguments than provided
  num.formals < num_args &&
    return(sprintf("Accepts only up to %d arguments", num.formals))

  ## skip deprecated functions
  ignore_deprecated && any(grepl("\\.Deprecated", body(fun))) &&
    return("Deprecated function")

  fun
}

#' Generate coloured summary statistics from the results
#'
#' This computes summary statistics from the fuzzing results, prints a
#' message for the overall success or failure, and returns a summary string.
#'
#' @param object An object of class `cbtf`.
#' @param verbose Whether a message on the overall pass or fail of the fuzz
#'        run should be printed out (`TRUE` by default).
#'
#' @return
#' A summary results string formatted with ANSI colour codes.
#'
#' @noRd
compute_summary_stats <- function(object, verbose = TRUE) {
  results <- unlist(lapply(object$runs, function(x) x$res))
  success <- sum(results %in% c("FAIL", "WARN")) == 0
  if (verbose) {
    use.utf8 <- cli::is_utf8_output()
    symb.succ <- if (use.utf8) "\U1F3C3" else " "
    symb.fail <- if (use.utf8) "\U1F6A8" else " "
    if (success)
      cli::cli_alert_success(" {symb.succ} You didn't get caught by the fuzz!")
    else
      cli::cli_alert_danger(" {symb.fail}   CAUGHT BY THE FUZZ!   {symb.fail}")
  }

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
#' @param res A character vector with "OK", "SKIP", "WARN" or "FAIL".
#' @param num A numerical value: if it evaluates to a positive finite value
#'        and `colour` is `TRUE`, then the string is coloured. The default
#'        value (`Inf`) implies that colour is applied, but `num` is not
#'        printed out.
#' @param colour A logical value that determines if the colour should be
#'        applied. If `FALSE`, nothing gets coloured independently of `num`.
#'
#' @return
#' A character vector formatted with ANSI colour codes.
#'
#' @noRd
tocolour <- function(res, num = Inf, colour = TRUE) {
  if (num > 0 && colour) {
    cols <- list(FAIL = cli::col_yellow("FAIL"),
                 WARN = cli::col_magenta("WARN"),
                 SKIP = cli::col_blue("SKIP"),
                 OK   = cli::col_green("OK"))
    res <- mapply(function(x) cols[[x]], res)
  }
  paste0(res, if (!is.infinite(num)) sprintf(" %d", num))
}

#' Append to each input a listified call to that input
#'
#' @param input A named list.
#'
#' @return
#' A named list with the original elements followed by their listified
#' version.
#'
#' @noRd
append_listified <- function(input) {
  transformed <- lapply(input, list)
  if (any(nzchar(names(transformed))))
    names(transformed) <- sprintf("list(%s)", names(transformed))
  c(input, transformed)
}

#' Retrieve or generate names for all elements in a list
#'
#' @param input A list of possibly named elements.
#'
#' @return
#' A character vector of names.
#'
#' @noRd
get_element_names <- function(input) {
  mapply(function(name, value) {
    !is.null(name) && nzchar(name) && return(name)
    deparse(value)[1]
  }, names(input) %||% "", input, USE.NAMES = FALSE)
}

#' Create a list of argument lists modified according to the given inputs
#'
#' It is a precondition that `args` and `what` cannot be both `NULL`.
#'
#' @param args A named list of arguments.
#' @param what A named list of inputs.
#'
#' @return
#' A list of named lists with the given arguments in turn replaced by each of
#' the inputs. List elements with the same names are removed.
#'
#' @noRd
modify_args <- function(args, what) {
  is.null(args) && return(lapply(what, list))
  nm <- paste(sapply(args, deparse, control = "nice", nlines = 1), collapse = ", ")
  is.null(what) && return(setNames(list(unname(args)), nm))
  names.what <- names(what)
  names.args <- names(args)
  res <- unlist(lapply(seq_along(args), function(idx) {
    new <- lapply(what, function(inp) {
      new <- unname(args)
      new[idx] <- list(inp)
      new
    })
    names(new) <- vapply(seq_along(what), function(i) {
      new <- names.args
      new[idx] <- names.what[i]
      paste(new, collapse = ", ")
    }, character(1))
    new
  }), recursive = FALSE)

  ## keep unique elements according to their names
  res[!duplicated(names(res))]
}
