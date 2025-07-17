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
#'        namespace; otherwise, the functions are searched in the global
#'        namespace.
#' @param what The object to be passed as first argument to each of the
#'        functions in `funs`.
#' @param package A character string specifying the name of the package to
#'        search for function names. If `NULL` (default), the function will
#'        first attempt to use the `"package"` attribute of `funs`, and if
#'        that is not set, names will be searched in the global namespace.
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
fuzz <- function(funs, what, package = NULL,
                 ignore.patterns = NULL, ignore.warnings = FALSE) {
  report <- function(label, msg) {
    out.res[[idx]]["res"] <<- label
    out.res[[idx]]["msg"] <<- gsub("\\n", " ", msg) # shorten multiline messages
  }

  ## input validation
  validate_class(funs, "character")
  validate_not_missing(what)

  ## define where functions names are searched
  if (is.null(package))
    package <- attr(funs, "package")
  getter <- if (is.null(package)) get else {
    validate_class(package, "character")
    function(x) utils::getFromNamespace(x, package)
  }

  ignore.patterns <- paste0(c(ignore.patterns,
                              "is missing, with no default"),
                            collapse = "|")
  out.res <- lapply(funs, function(x) {
    data.frame(fun = x, res = "OK", msg = "")
  })

  ## string representation of the input
  what.char <- deparse(substitute(what))

  ## loop over the functions to fuzz
  cli::cli_progress_bar(paste("Fuzzing input:", what.char), total = length(funs))
  for (idx in seq_along(funs)) {
    f <- funs[idx]
    fun <- validate_fuzzable(try(getter(f), silent = TRUE), package)
    if (is.character(fun)) {
      report("SKIP", fun)
      cli::cli_progress_update()
      next
    }

    tryCatch(utils::capture.output(suppressMessages(fun(what))),
             error = function(e) {
               if (!grepl(f, e) &&
                   !grepl(ignore.patterns, e)) {
                 report("FAIL", e$message)
               }
             },
             warning = function(w) {
               if (!ignore.warnings &&
                   !grepl(f, w) &&
                   !grepl(ignore.patterns, w)) {
                 report("WARN", w$message)
               }
             })
    cli::cli_progress_update()
  }
  cli::cli_progress_done()

  ## transform results to a data frame
  out.res <- as.data.frame(do.call(rbind, out.res))
  attr(out.res, "what") <- what.char

  ## complete the returned object
  structure(list(runs = list(out.res),
                 package = if (!is.null(package)) package else NA),
            class = "cbtf")
}
