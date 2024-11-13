##===========================================================================
##
## Copyright (c) 2024 Marco Colombo
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

#' Get the names of the exported functions of a package
#'
#' @param package Name of the package to fuzz-test.
#' @param ignore.names Names of functions to ignore: unless set to `NULL`
#' (default), these are removed from the names returned.
#'
#' @return
#' A character vector of the names of the functions exported from the
#' requested package, with the `"package"` attribute set.
#'
#' @export
get_exported_functions <- function(package, ignore.names = NULL) {
  funs <- sort(getNamespaceExports(package))
  funs <- grep(".__", funs, fixed = TRUE, invert = TRUE, value = TRUE)
  funs <- setdiff(funs, ignore.names)
  attr(funs, "package") <- package
  return(funs)
}

#' Fuzz-test the specified functions
#'
#' This function calls each of the functions it receives with the object
#' specified in `what`.
#'
#' @param funs A character vector of function names to test. If a `"package"`
#'        attribute is set, the functions are loaded directly from the package
#'        namespace; otherwise, the functions are searched in the workspace.
#' @param what The object to be passed as first argument to each of the
#'        functions in `funs`.
#' @param ignore.patterns A character string containing a regular expression
#'        to match the messages to ignore.
#' @param ignore.warnings Whether warnings should be ignored (`FALSE` by
#'        default).
#'
#' @export
fuzz <- function(funs, what, ignore.patterns = NULL,
                 ignore.warnings = FALSE) {
  header <- function(count) {
    if (count > 0) return()
    cat("\n\t\U0001f6a8   CAUGHT BY THE FUZZ!   \U0001f6a8\n\n")
  }
  footer <- function(count) {
    if (count > 0) return(invisible())
    cat("\U0001f3c3 You didn't get caught by the fuzz!\n")
  }
  getter <- function() {
    if (is.null(package))
      get
    else
      function(x) utils::getFromNamespace(x, package)
  }

  count <- 0
  ignore.patterns <- paste0(c(ignore.patterns,
                              "is missing, with no default"),
                            collapse = "|")
  package <- attr(funs, "package")
  for (f in funs) {
    fun <- getter()(f)
    tryCatch(utils::capture.output(suppressMessages(fun(what))),
             error = function(e) {
               if (!grepl(f, e) &&
                   !grepl(ignore.patterns, e)) {
                 header(count)
                 cat("FAIL:", f, "(", class(what), ")\n   ", e$message, "\n\n")
                 count <<- count + 1
               }
             },
             warning = function(w) {
               if (!ignore.warnings && !grepl(ignore.patterns, w)) {
                 header(count)
                 cat("WARN:", f, "\n   ", w$message, "\n\n")
                 count <<- count + 1
               }
             })
  }
  footer(count)
}
