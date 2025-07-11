##===========================================================================
##
## Copyright (c) 2024-2025 Marco Colombo
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
#' @return
#' A list of class `cbtf` that stores the results obtained for each of the
#' functions tested.
#'
#' @export
fuzz <- function(funs, what, ignore.patterns = NULL,
                 ignore.warnings = FALSE) {
  header <- function(count) {
    if (count > 0) return()
    cat("\n\t\U0001f6a8   CAUGHT BY THE FUZZ!   \U0001f6a8\n\n")
  }
  footer <- function(count) {
    if (count > 0) return()
    cat("\U0001f3c3 You didn't get caught by the fuzz!\n")
  }
  report <- function(label, msg, count) {
    out.res[[idx]]["res"] <<- label
    out.res[[idx]]["msg"] <<- msg
    header(count)
    cat(paste0(label, ":"), f, "(", class(what), ")\n   ", msg, "\n\n")
    count <<- count + 1
  }
  getter <- function() {
    if (is.null(package))
      get
    else
      function(x) utils::getFromNamespace(x, package)
  }

  ## input validation
  validate_class(funs, "character")
  validate_not_missing(what)

  count <- 0
  ignore.patterns <- paste0(c(ignore.patterns,
                              "is missing, with no default"),
                            collapse = "|")
  package <- attr(funs, "package")
  out.res <- lapply(funs, function(x) {
    ## the default result is SKIP as `funs` may contain non-functions
    data.frame(fun = x, res = "SKIP", msg = "")
  })

  ## loop over the functions to fuzz
  for (idx in seq_along(funs)) {
    f <- funs[idx]

    ## skip non-existing names
    fun <- try(getter()(f), silent = TRUE)
    if(inherits(fun, "try-error")) {
      next
    }

    ## skip non-functions
    if (!is.function(fun) || contains.readline(fun))
      next

    out.res[[idx]]["res"] <- "OK"
    tryCatch(utils::capture.output(suppressMessages(fun(what))),
             error = function(e) {
               if (!grepl(f, e) &&
                   !grepl(ignore.patterns, e)) {
                 report("FAIL", e$message, count)
               }
             },
             warning = function(w) {
               if (!ignore.warnings && !grepl(ignore.patterns, w)) {
                 report("WARN", w$message, count)
               }
             })
  }
  footer(count)

  ## complete the returned object
  attr(out.res, "what") <- deparse(as.list(match.call())$what)
  attr(out.res, "package") <- package
  class(out.res) <- "cbtf"
  invisible(out.res)
}
