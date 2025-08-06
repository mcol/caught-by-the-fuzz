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

#' Results summary from a fuzz run
#'
#' Reports some summary statistics from the results of a run of [fuzz].
#'
#' @param object An object of class `cbtf`.
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
summary.cbtf <- function(object, ...) {
  df <- cbind(do.call(rbind, object$runs),
              what = sapply(object$runs, function(x) attr(x, "what")))
  cli::cli_text("Fuzzed {nrow(object$runs[[1]])} function{?s} ",
                "on {length(object$runs)} input{?s}: ")
  print(tbl <- table(df$fun,
                     factor(df$res, levels = c("FAIL", "WARN", "SKIP", "OK"))))
  cat("\n", compute_summary_stats(object, verbose = FALSE), "\n", sep = "")
  invisible(structure(df[, c("fun", "what", "res", "msg")],
                      summary_table = tbl))
}

#' Print the results from a fuzz run
#'
#' This formats with colours the results from a fuzz run and prints them to
#' the terminal.
#'
#' @param x An object of class `cbtf`.
#' @param show_all Whether all results should be printed. By default (`FALSE`),
#'        only the functions that reported an error or a warning are printed.
#'        If `TRUE`, all functions tested are printed, including those that
#'        were successful or were skipped.
#' @param ... Further arguments passed to or from other methods.
#'        These are currently ignored.
#'
#' @return
#' No return value, called for side effects.
#'
#' @examples
#' res <- fuzz(funs = c("list", "matrix", "mean"),
#'             what = test_inputs(c("numeric", "raw")))
#' print(res, show.all = TRUE)
#'
#' @seealso [summary.cbtf]
#'
#' @export
print.cbtf <- function(x, show_all = FALSE, ...) {
  summary.stats <- compute_summary_stats(x)
  res.size <- nchar(tocolour("FAIL")) # include ANSI formatting, if present
  for (run in x$runs) {
    if (!show_all)
      run <- run[run$res %in% c("FAIL", "WARN"), ]
    if (nrow(run) > 0) {
      cli::cli_h3(paste("Test input:", cli::style_bold(attributes(run)$what)))
      max.name <- max(c(0, nchar(run$fun))) + 1
      cat(sprintf("%*s  %*s  %s\n",
                  max.name, run$fun, res.size, tocolour(run$res), run$msg),
          sep = "")
    }
  }
  cat("\n", summary.stats, "\n")
}

#' Compute the number of inputs tested
#'
#' @param x An object of class `cbtf`.
#'
#' @return
#' An integer corresponding to the number of inputs tested in a run.
#'
#' @examples
#' res <- fuzz(funs = c("list", "matrix", "mean"),
#'             what = test_inputs(c("numeric", "raw")))
#' length(res)
#'
#' @export
length.cbtf <- function(x) {
  length(x$runs)
}
