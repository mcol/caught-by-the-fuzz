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

#' Check if the body of a function contains calls to readline()
#'
#' @param fun An expression.
#'
#' @return
#' A logical value.
#'
#' @noRd
contains.readline <- function(expr) {
  if (is.function(expr))
    expr <- body(expr)
  any(sapply(expr, function(line) {
    if (length(line) > 1)
      return(contains.readline(line))
    any(grepl("^readline", deparse(line)))
  }))
}
