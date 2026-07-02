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

#' Results summary from a fuzz run
#'
#' Reports some summary statistics from the results of a run of [fuzz].
#'
#' The use of unicode icons in the output messages can be disabled by setting
#' `options(cli.unicode = FALSE)`.
#'
#' @param object An object of class `cbtf`.
#' @param tabulate Whether a tabulation of results should be printed out
#'        (`TRUE` by default). The tabulation can always be retrieved from
#'        the `"summary_table"` attribute of the returned object also when
#'        `tabulate = FALSE`.
#' @param ... Further arguments passed to or from other methods.
#'        These are currently ignored.
#'
#' @return
#' A data frame containing the following columns and attributes is returned
#' invisibly:
#' \item{fun}{The names of the function tested.}
#' \item{what}{The inputs tested.}
#' \item{res}{One of "OK", "FAIL", "WARN" or "SKIP" for each combination of
#'       function and input tested (see the *Value* section in [fuzz]).}
#' \item{msg}{The message received in case of error, warning or skip,
#'       or an empty string if no failure occurred.}
#' \item{attr(*, "summary_table")}{The tabulation of results that was printed
#'       out.}
#'
#' @examples
#' res <- fuzz(funs = c("list", "matrix", "mean"),
#'             what = test_inputs(c("numeric", "raw")))
#' summary(res)
#'
#' @seealso [print.cbtf]
#'
#' @export
summary.cbtf <- function(object, tabulate = TRUE, ...) {
  validate_class(tabulate, "logical", scalar = TRUE, from = "summary")
  df <- cbind(do.call(rbind, object$runs),
              fun = rep(object$funs, length(object$runs)),
              what = sapply(object$runs, function(x) attr(x, "what")))
  cli::cli_text("Fuzzed {nrow(object$runs[[1]])} function{?s} ",
                "on {length(object$runs)} input{?s}: ")
  tbl <- table(df$fun,
               factor(df$res, levels = c("FAIL", "WARN", "SKIP", "OK")))
  if (tabulate)
    print(tbl)
  cat("\n", compute_summary_stats(object, verbose = FALSE), "\n", sep = "")
  invisible(structure(df[, c("fun", "what", "res", "msg")],
                      summary_table = tbl))
}

#' Print the results from a fuzz run
#'
#' This formats the results from a fuzz run with colours and prints them to
#' the terminal.
#'
#' The use of unicode icons in the output messages can be disabled by setting
#' `options(cli.unicode = FALSE)`.
#'
#' @param x An object of class `cbtf`.
#' @param show A character vector representing the subset of results to be
#'        printed, any of "fail", "warn", "skip", "ok", "all" and "none".
#' @param group Either `"input"` to show results grouped by test input
#'        (default), or `"function"` to show them grouped by function name.
#' @param ... Further arguments passed to or from other methods.
#'        These are currently ignored.
#'
#' @return
#' No return value, called for side effects.
#'
#' @examples
#' res <- fuzz(funs = c("list", "matrix", "mean"),
#'             what = test_inputs(c("numeric", "raw")))
#' print(res)
#' print(res, show = "all")
#' print(res, show = "none")
#' print(res, group = "function")
#'
#' @seealso [summary.cbtf]
#'
#' @export
print.cbtf <- function(x, show = c("fail", "warn"), group = "input", ...) {
  validate_class(show, "character", from = "print")
  validate_class(group, "character", scalar = TRUE, from = "print")
  show <- tolower(show)
  if ("none" %in% show) {
    cat(compute_summary_stats(x, verbose = FALSE), "\n")
    return(invisible())
  }
  if ("all" %in% show)
    show <- c("fail", "warn", "skip", "ok")
  group <- match.arg(group, c("input", "function"))
  summary.stats <- compute_summary_stats(x)
  res.size <- nchar(tocolour("FAIL"))

  ## Build a long-format data frame with all results
  df <- do.call(rbind, lapply(x$runs, function(run) {
    cbind(run, fun = x$funs, what = attr(run, "what"))
  }))

  if (group == "function") {
    for (fun in x$funs) {
      sub <- df[df$fun == fun & tolower(df$res) %in% show, , drop = FALSE]
      if (nrow(sub) == 0) next
      cli::cli_h3("Function {.strong {.code {fun}}}:")
      msg.size <- max(nchar(sub$msg), 30)
      cat(sprintf(" %*s  %-*s | %s\n",
                  res.size, tocolour(sub$res), msg.size, sub$msg, sub$what),
          sep = "")
    }
  } else {
    max.name <- max(nchar(x$funs)) + 1
    for (ii in seq_along(x$runs)) {
      what <- attr(x$runs[[ii]], "what")
      sub <- df[df$what == what & tolower(df$res) %in% show, , drop = FALSE]
      if (nrow(sub) == 0) next
      cli::cli_h3("Test input [[{ii}]]: {.strong {what}}")
      cat(sprintf("%*s  %*s  %s\n",
                  max.name, sub$fun, res.size, tocolour(sub$res), sub$msg),
          sep = "")
    }
  }
  cat("\n", summary.stats, "\n")
}

#' Extract the results for a specific test input
#'
#' @param x An object of class `cbtf`.
#' @param i An index between 1 and the number of test inputs used.
#'
#' @return
#' If the index is valid, a data frame containing the following columns and
#' attributes:
#' \item{res}{One of "OK", "FAIL", "WARN" or "SKIP" for each combination of
#'       function and input tested (see the *Value* section in [fuzz]).}
#' \item{msg}{The message received in case of error, warning or skip,
#'       or an empty string if no failure occurred.}
#' \item{attr(*, "what")}{The character representation of the input tested.}
#' Otherwise, `FALSE`.
#'
#' @examples
#' res <- fuzz(funs = c("list", "matrix", "mean"),
#'             what = test_inputs(c("numeric", "raw")))
#' res[[6]]
#'
#' @export
`[[.cbtf` <- function(x, i) {
  i %in% seq_along(x$runs) && return(x$runs[[i]])
}

#' Compute the number of tests performed
#'
#' @param x An object of class `cbtf`.
#'
#' @return
#' An integer corresponding to the number of tests performed in a run.
#'
#' @examples
#' res <- fuzz(funs = c("list", "matrix", "mean"),
#'             what = test_inputs(c("numeric", "raw")))
#' length(res)
#'
#' @export
length.cbtf <- function(x) {
  length(x$runs) * length(x$runs[[1]]$res)
}
