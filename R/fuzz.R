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
  validate_class(package, "character", from = "get_exported_functions")
  if (!is.null(ignore.names))
    validate_class(ignore.names, "character", from = "get_exported_functions")
  funs <- sort(getNamespaceExports(package))
  funs <- grep(".__", funs, fixed = TRUE, invert = TRUE, value = TRUE)
  funs <- setdiff(funs, ignore.names)
  attr(funs, "package") <- package
  return(funs)
}

#' Fuzz-test the specified functions
#'
#' This function calls each of the functions it receives with each of the
#' objects specified in `what`.
#'
#' @param funs A character vector of function names to test. If a `"package"`
#'        attribute is set, the functions are loaded directly from the package
#'        namespace; otherwise, the functions are searched in the global
#'        namespace.
#' @param what A list objects to be passed, one at a time, as first argument
#'        to each of the functions in `funs`. Ideally, the list should be
#'        named, allowing each input tested to be pretty-printed with the name
#'        provided. If the list is not named, a deparsed representation of the
#'        input will be displayed, which may look unwieldy in some cases.
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
#' An object of class `cbtf` that stores the results obtained for each of the
#' functions tested.
#'
#' @export
fuzz <- function(funs, what, package = NULL,
                 ignore.patterns = NULL, ignore.warnings = FALSE) {

  ## input validation
  validate_class(funs, "character")
  validate_class(what, "list")
  if (is.null(package)) {
    package <- attr(funs, "package")
  } else {
    validate_class(package, "character")
  }

  ## loop over the inputs
  runs <- list()
  cli::cli_alert_info("Fuzzing {length(funs)} function{?s} on {length(what)} input{?s}")
  what.chars <- names(what)
  for (idx in seq_along(what)) {
    ## string representation of the input
    what.char <- what.chars[idx]
    if (is.null(what.char))
      what.char <- deparse(what[[idx]])[[1]]

    ## report progress if running interactively
    if (cli::is_dynamic_tty())
      cli::cli_progress_step(paste("Fuzzing input:", what.char))

    ## fuzz this input
    runs[[idx]] <- fuzzer(funs, what[[idx]], what.char, package,
                          ignore.patterns, ignore.warnings)
  }
  cli::cli_progress_done()

  ## returned object
  structure(list(runs = runs,
                 package = if (!is.null(package)) package else NA),
            class = "cbtf")
}

#' Fuzzing engine
#'
#' This is where the actual fuzzing happens. This function supports only one
#' input, which is passed to each of the functions in `funs`.
#'
#' @param funs A character vector of function names to test.
#' @param what The object to be passed as first argument to each of the
#'        functions in `funs`.
#' @param what.char A string representation of the input in `what`, used for
#'        pretty-printing the output.
#' @param package A character string specifying the name of the package to
#'        search for function names.
#' @param ignore.patterns A character string containing a regular expression
#'        to match the messages to ignore.
#' @param ignore.warnings Whether warnings should be ignored (`FALSE` by
#'        default).
#'
#' @return
#' A data.frame of results obtained for each of the functions tested, with
#' the attribute `what` set to contain the string representation of the input
#' tested.
#'
#' @noRd
fuzzer <- function(funs, what, what.char = "", package = NULL,
                   ignore.patterns = NULL, ignore.warnings = FALSE) {
  report <- function(label, msg) {
    out.res[[idx]]["res"] <<- label
    out.res[[idx]]["msg"] <<- gsub("\\n", " ", msg) # shorten multiline messages
  }

  ## define where functions names are searched
  getter <- if (is.null(package)) get else {
    function(x) utils::getFromNamespace(x, package)
  }

  ignore.patterns <- paste0(c(ignore.patterns,
                              "is missing, with no default"),
                            collapse = "|")
  out.res <- lapply(funs, function(x) {
    data.frame(fun = x, res = "OK", msg = "")
  })

  ## loop over the functions to fuzz
  cli::cli_progress_bar(paste(cli::col_br_blue("\U2139"), # ℹ
                              "Fuzzing input:", what.char),
                        total = length(funs))
  for (idx in seq_along(funs)) {
    f <- funs[idx]
    fun <- check_fuzzable(try(getter(f), silent = TRUE), package)
    if (is.character(fun)) {
      report("SKIP", fun)
      cli::cli_progress_update()
      next
    }

    cli::cli_progress_update(status = f)
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
  }
  cli::cli_progress_done()

  ## transform results to a data frame
  structure(as.data.frame(do.call(rbind, out.res)),
            what = what.char)
}
