##=============================================================================
##
## Copyright (c) 2025 Marco Colombo
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 2 of the License, or
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
##=============================================================================


#' CBTF: Caught by the Fuzz! A minimalistic fuzz-test runner
#'
#' This package implements a very simple mechanism for fuzz-testing functions
#' in the public interface of an R package.
#'
#' Fuzz testing helps identify functions lacking sufficient argument validation,
#' and uncovers sets of inputs that, while valid by function signature, may
#' cause issues within the function body.
#'
#' The core functionality of the package is [fuzz()], whose aim is to call
#' each provided function with a certain input and record the output produced.
#' If an error is generated, this is captured and reported to the user, unless
#' the error message matches a pattern of whitelisted errors.
#'
#' The helper function [get_exported_functions()] identifies the functions
#' in the public interface of a given package, facilitating the generation of
#' the list of functions to be fuzzed.
#'
#' @author
#' Marco Colombo \email{mar.colombo13@@gmail.com}
#'
"_PACKAGE"
