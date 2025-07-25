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
#' @param ignore_names Names of functions to ignore: these are removed from
#'        the names returned. This can be helpful, for example, to discard
#'        function aliases.
#'
#' @return
#' A character vector of the names of the functions exported from the
#' requested package, with the `"package"` attribute set.
#'
#' @export
get_exported_functions <- function(package, ignore_names = "") {
  validate_class(package, "character", from = "get_exported_functions")
  validate_class(ignore_names, "character", from = "get_exported_functions")
  funs <- sort(getNamespaceExports(package))
  funs <- grep(".__", funs, fixed = TRUE, invert = TRUE, value = TRUE)

  ## keep only functions
  keep.idx <- sapply(funs, function(x) {
    is.function(utils::getFromNamespace(x, package))
  })
  funs <- setdiff(funs[keep.idx], ignore_names)
  attr(funs, "package") <- package
  return(funs)
}

#' Fuzz-test the specified functions
#'
#' This function calls each of the functions it receives with each of the
#' objects specified in `what`.
#'
#' @param funs A character vector of function names to test. If a `"package"`
#'        attribute is set, the functions are loaded from that package's
#'        namespace; otherwise, they are searched in the global namespace.
#' @param what A list of objects to be passed, one at a time, as the first
#'        argument to each function in `funs`. Ideally, the list should be
#'        named, so that each input tested can be pretty-printed with the
#'        corresponding name. For unnamed lists, a deparsed representation of
#'        the inputs will be used, which may appear unwieldy in some cases.
#'        If nothing is provided, a default set of inputs will be used.
#' @param package A character string specifying the name of the package to
#'        search for functions. If `NULL` (default), the function will first
#'        check the `"package"` attribute of `funs`, and if that is not set,
#'        names will be searched in the global namespace.
#' @param listify_what Whether each input in `what` should also be tested
#'        in its listified version (`FALSE` by default). When set to `TRUE`,
#'        if `what` is `list(x = x)`, the function will operate as if `what`
#'        were `list(x = x, "list(x)" = list(x))`, for any input object `x`.
#' @param ignore_patterns One or more strings containing regular expressions
#'        to match the errors to ignore. The string "is missing, with no
#'        default" is always ignored.
#' @param ignore_warnings Whether warnings should be ignored (`FALSE` by
#'        default).
#'
#' @return
#' An object of class `cbtf` that stores the results obtained for each of the
#' functions tested.
#'
#' @export
fuzz <- function(funs, what = input_list, package = NULL, listify_what = FALSE,
                 ignore_patterns = "", ignore_warnings = FALSE) {

  ## input validation
  validate_class(funs, "character")
  validate_class(what, "list")
  if (is.null(package)) {
    package <- attr(funs, "package")
  } else {
    validate_class(package, "character")
  }
  validate_class(listify_what, "logical")
  validate_class(ignore_patterns, "character")

  ## expand the set of inputs with their listified version
  if (listify_what)
    what <- append_listified(what)

  ## join all regular expression patterns
  ignore_patterns <- paste0(c(ignore_patterns,
                              "is missing, with no default"),
                            collapse = "|")
  ignore_patterns <- gsub("^\\|", "", ignore_patterns) # remove extra |

  ## loop over the inputs
  runs <- list()
  cli::cli_alert_info(c("Fuzzing {length(funs)} function{?s} ",
                        "on {length(what)} input{?s}"))
  what.chars <- names(what)
  for (idx in seq_along(what)) {
    ## string representation of the input
    what.char <- what.chars[idx]
    if (is.null(what.char))
      what.char <- deparse(what[[idx]])[[1]]

    ## report progress if running interactively
    if (cli::is_dynamic_tty())
      cli::cli_progress_step(paste("Fuzzing input:", what.char)) # nocov

    ## fuzz this input
    runs[[idx]] <- fuzzer(funs, what[[idx]], what.char, package,
                          ignore_patterns, ignore_warnings)
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
#' @param what The object to be passed as the first argument to each of the
#'        functions in `funs`.
#' @param what_char A string representation of the input in `what`, used for
#'        pretty-printing the output.
#' @param package A character string specifying the name of the package to
#'        search for function names.
#' @param ignore_patterns A character string containing a regular expression
#'        to match the messages to ignore.
#' @param ignore_warnings Whether warnings should be ignored (`FALSE` by
#'        default).
#'
#' @return
#' A data.frame of results obtained for each of the functions tested, with
#' the attribute `what` set to contain the string representation of the input
#' tested.
#'
#' @noRd
fuzzer <- function(funs, what, what_char = "", package = NULL,
                   ignore_patterns = "is missing, with no default",
                   ignore_warnings = FALSE) {
  report <- function(label, msg) {
    out.res[[idx]]["res"] <<- label
    out.res[[idx]]["msg"] <<- gsub("\\n", " ", msg) # shorten multiline messages
  }

  ## define where functions names are searched
  getter <- if (is.null(package)) get else {
    function(x) utils::getFromNamespace(x, package)
  }

  out.res <- lapply(funs, function(x) {
    data.frame(fun = x, res = "OK", msg = "")
  })

  ## loop over the functions to fuzz
  cli::cli_progress_bar(paste(cli::col_br_blue("\U2139"), # ℹ
                              "Fuzzing input:", what_char),
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
                   !grepl(ignore_patterns, e)) {
                 report("FAIL", e$message)
               }
             },
             warning = function(w) {
               if (!ignore_warnings &&
                   !grepl(f, w) &&
                   !grepl(ignore_patterns, w)) {
                 report("WARN", w$message)
               }
             })
  }
  cli::cli_progress_done()

  ## transform results to a data frame
  structure(as.data.frame(do.call(rbind, out.res)),
            what = what_char)
}
