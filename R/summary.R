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
#' Generates a data frame of summary results from a single run of [fuzz()].
#'
#' @param object An object of class `cbtf`.
#' @param ... Further arguments passed to or from other methods.
#'        These are currently ignored.
#'
#' @return
#' A data frame with the following columns:
#' \item{fun}{The names of the function tested.}
#' \item{res}{One of "OK", "FAIL", "WARN" or "SKIP" for each function
#'       tested.}
#' \item{msg}{The message received in case of error, warning or skip,
#'       or an empty string if the function succeeded.}
#'
#' @export
summary.cbtf <- function(object, ...) {
  as.data.frame(do.call(rbind, object$runs))
}

#' Print the results from a fuzz run
#'
#' @param x An object of class `cbtf`.
#' @param show.all Whether all results should be printed. By default (`FALSE`),
#'        only the functions that reported an error or a warning are printed.
#'        If `TRUE`, all functions tested are printed, including those that
#'        were skipped.
#' @param ... Further arguments passed to or from other methods.
#'        These are currently ignored.
#'
#' @export
print.cbtf <- function(x, show.all = FALSE, ...) {
  summary <- summary(x)
  if (!show.all) {
    summary <- summary[summary$res %in% c("FAIL", "WARN"), ]
    row.names(summary) <- NULL
  }
  print(summary)
}
