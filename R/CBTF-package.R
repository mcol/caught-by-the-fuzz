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
#' The core functionality of the package is in [fuzz], which calls each
#' provided function with a certain input and records the output produced.
#' If an error or a warning is generated, this is captured and reported to the
#' user, unless it matches a pattern of whitelisted messages. The objects
#' returned by [fuzz] can be inspected with [summary.cbtf] and [print.cbtf].
#'
#' Whitelisting can also be done after a fuzz run has been completed via the
#' [whitelist] function, so that only messages that need to be acted upon are
#' actually shown. Using [whitelist] has the advantage of not requiring the
#' completion of a fuzz run of all functions over all inputs again.
#'
#' The helper function [get_exported_functions] identifies the functions
#' in the public interface of a given package, facilitating the generation of
#' the list of functions to be fuzzed.
#'
#' The helper function [test_inputs] is invoked by [fuzz] if the user doesn't
#' specify the set of inputs to be tested. By default generates a large set of
#' potentially problematic inputs, but these can be limited just to the
#' desired classes of inputs.
#'
"_PACKAGE"
