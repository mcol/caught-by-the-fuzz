##===========================================================================
##
## Copyright (c) 2024-2026 Marco Colombo
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
#' This function extracts the exports from the namespace of the given package
#' via [getNamespaceExports] and discards non-fuzzable objects (non-functions
#' and functions with no arguments). The set of names returned can be further
#' restricted via the `ignore_names` and `ignore_deprecated` arguments.
#'
#' @param package Name of the package to fuzz-test.
#' @param ignore_names Names of functions to ignore: these are removed from
#'        the names returned. This can be helpful, for example, to discard
#'        function aliases.
#' @param ignore_deprecated Whether deprecated function should be ignored
#'        (`TRUE` by default).
#'
#' @return
#' A character vector of the names of the fuzzable functions exported from
#' the given package, with the `"package"` attribute set. This can be used
#' directly as the `funs` argument of [fuzz] without need to specify the
#' `package` argument.
#'
#' @examples
#' ## get the fuzzable functions in the public interface of this package
#' funs <- get_exported_functions("CBTF")
#'
#' @seealso [fuzz]
#'
#' @export
get_exported_functions <- function(package, ignore_names = "",
                                   ignore_deprecated = TRUE) {
  from <- "get_exported_functions"
  validate_class(package, "character", from = from,
                 scalar = TRUE, remove_empty = TRUE)
  validate_class(ignore_names, "character", from = from)
  validate_class(ignore_deprecated, "logical", from = from, scalar = TRUE)
  funs <- tryCatch(sort(getNamespaceExports(package)),
                   error = function(e) fuzz_error(e$message, from = from))

  ## keep only fuzzable functions
  keep.idx <- sapply(funs, function(x) {
    is.function(check_fuzzable(x, package, ignore_deprecated))
  })
  funs <- setdiff(funs[keep.idx], ignore_names)
  attr(funs, "package") <- package
  funs
}

#' Fuzz-test the specified functions
#'
#' The fuzzer calls each function in `funs` with the argument list provided in
#' `args` (each of its elements in turn modified by each object in `what`) and
#' records any errors or warnings that are thrown. If no error occurs within
#' the first `timeout` seconds, the execution of the function being fuzzed is
#' interrupted and the next one is started.
#'
#' @details
#' ## Multiple arguments
#'
#' An list of arguments to be passed to the functions being fuzzed can be
#' provided via the `args` argument. Each element in that list is modified in
#' turn by each object in `what` and the resulting list of arguments is then
#' passed to each function via `do.call()`. If more arguments are given than
#' the number formal arguments accepted by a function, that function will
#' produce a "SKIP" result.
#'
#' ## Parallel execution
#'
#' The implementation uses `mirai` as a backend to execute tasks asynchronously
#' in parallel worker processes. The function can start a pool of persistent
#' background processes (daemons) of size given by the `daemons` argument
#' (note that starting more daemons than available cores yields no benefit).
#' Alternatively, the function can also make use of already active daemons
#' started with the [mirai::daemons] function: this allows to control in
#' greater detail the number of processes to use, which can also be remote.
#'
#' ## Whitelisting
#'
#' In order to reduce the number of false positive results produced, this
#' function applies the following set rules, to establish if an error or
#' warning condition should ignored (whitelisting):
#'
#' * If the name of the function appears in the error or warning message, as
#'   it is considered that the condition has been handled by the developer.
#' * If the error or warning message contains the text "is missing, with no
#'   default", which is produced when a missing argument is used without a
#'   value being assigned to it.
#' * If the error or warning message contains any of the patterns specified
#'   in `ignore_patterns`.
#' * If a warning is thrown but `ignore_warnings = TRUE` is set.
#'
#' In all whitelisted cases, the result is "OK", and the message that
#' was received is stored in the `$msg` field (see the *Value* section).
#'
#' *Note:* Whitelisting can also be applied post-hoc on the results of a fuzz
#' run using the [whitelist] function.
#'
#' @param funs A character vector of function names to test. If a `"package"`
#'        attribute is set and is no `package` argument is provided, functions
#'        are loaded from the namespace specified in the attribute.
#' @param what A list of objects; each is used, in turn, to modify the list of
#'        arguments in `args` before calling each of the functions in `funs`.
#'        If no inputs are provided, a default set of inputs generated by
#'        [test_inputs] will be used. If set to `NULL`, then `args` must be
#'        specified, and all functions will be called with that exact list of
#'        arguments with no fuzzing occurring.
#' @param args A list of default values for function arguments. Each argument
#'        in the list is in turn replaced by each element of `what`, then each
#'        modified argument list is used to fuzz the functions in `funs`. If
#'        `NULL` (default), only the first argument of each function is fuzzed.
#' @param package A character string specifying the name of the package to
#'        search for functions. If `NULL` (default), the function will first
#'        check the `"package"` attribute of `funs`, and if that is not set,
#'        names will be searched in the global namespace.
#' @param listify_what Whether each input in `what` should also be tested
#'        in its listified version (`FALSE` by default). When set to `TRUE`,
#'        if `what` is `list(x = x)`, the function will operate as if it
#'        were `list(x = x, "list(x)" = list(x))`, for any input object `x`.
#' @param ignore_patterns One or more strings containing regular expressions
#'        to match the errors to ignore. The string "is missing, with no
#'        default" is always ignored.
#' @param ignore_warnings Whether warnings should be ignored (`FALSE` by
#'        default).
#' @param daemons Number of daemons to use (2 by default). As many `mirai`
#'        daemons as specified will be started when entering the function and
#'        closed at the end, unless active daemons are already available, in
#'        which case the argument is ignored and the active daemons are used.
#' @param timeout Number of seconds (2 by default) after which the function
#'        being fuzzed is interrupted with result status set to "OK".
#'
#' @return
#' An object of class `cbtf` that stores the results obtained for each of the
#' functions tested. This contains the following fields:
#' \item{runs}{a list of data frames, each containing the results of fuzzing
#'       all the functions in `funs` with one of the inputs in `what`. The
#'       data frame contains the following columns and attributes:\cr
#'       - `res`: The result of the fuzz test, see below for the possible
#'         values.\cr
#'       - `msg`: The error or warning message returned by the function, if
#'          any.\cr
#'       - `attr(*, "what")`: The character representation of the input
#'         tested.
#' }
#' \item{funs}{a vector of names of the functions tested.}
#' \item{args}{a named list of arguments, with names generated by deparsing
#'       the `args` argument if not already provided.}
#' \item{package}{a character string specifying the package name where
#'       function names were searched, or `NA` if none was provided.}
#' \item{ignore_patterns}{The value of the `ignore_patterns` argument.}
#' \item{ignore_warnings}{The value of the `ignore_warnings` argument.}
#'
#' The `res` column in each of the data frames in the `$runs` field can
#' contain the following values:
#' * **OK**: either no error or warning was produced (in which case, the `msg`
#'   entry is left blank), or it was whitelisted (in which case, the message
#'   received is stored in `msg`), or it was timed out (in which case, `msg`
#'   records that a timeout was applied).
#' * **SKIP**: no test was run, either because the given name cannot be found, or
#'   it doesn't correspond to a function, or the function accepts no arguments,
#'   or more arguments were provided than the function accepts;
#'   the exact reason is given in `msg`.
#' * **WARN**: a warning was thrown for which no whitelisting occurred and
#'   `ignore_warnings = FALSE`; its message is stored in `msg`.
#' * **FAIL**: an error was thrown for which no whitelisting occurred; its message
#'   is stored in `msg`.
#'
#' @examples
#' ## set up persistent background processes
#' mirai::daemons(2L)
#'
#' ## this should produce no errors
#' res <- fuzz(funs = c("list", "matrix", "mean"),
#'             what = test_inputs(c("numeric", "raw")))
#' summary(res)
#'
#' ## display all results even for successful tests
#' print(res, show_all = TRUE)
#'
#' ## this will catch an error (false positive)
#' fuzz(funs = "matrix",  what = test_inputs("scalar"))
#'
#' ## apply a whitelist pattern to remove the false positive
#' fuzz(funs = "matrix",  what = test_inputs("scalar"),
#'      ignore_patterns = "'data' must be of a vector type")
#'
#' ## close the background processes
#' mirai::daemons(0L)
#'
#' @seealso [get_exported_functions], [test_inputs], [whitelist],
#' [summary.cbtf], [print.cbtf]
#'
#' @export
fuzz <- function(funs, what = test_inputs(), args = NULL,
                 package = NULL, listify_what = FALSE,
                 ignore_patterns = "", ignore_warnings = FALSE,
                 daemons = 2L, timeout = 2) {

  ## defuse the user inputs
  what.quo <- rlang::enquo(what)
  args.quo <- rlang::enquo(args)

  ## input validation
  validate_class(funs, "character", remove_empty = TRUE)
  validate_class(what, "list", null.ok = TRUE)
  validate_class(args, "list", null.ok = TRUE)
  if (is.null(what) && is.null(args))
    fuzz_error("'what' and 'args' cannot be both NULL")
  if (is.null(package)) {
    package <- attr(funs, "package")
  } else {
    validate_class(package, "character", scalar = TRUE, remove_empty = TRUE)
  }
  validate_class(listify_what, "logical", scalar = TRUE)
  validate_class(ignore_patterns, "character")
  validate_class(ignore_warnings, "logical", scalar = TRUE)
  validate_class(daemons, c("integer", "numeric"), scalar = TRUE, min = 1)
  validate_class(timeout, c("numeric", "integer"), scalar = TRUE, min = 1)

  ## cap the timeout so that converting to milliseconds won't overflow
  timeout <- min(timeout, 10000)

  ## start as many daemons as specified by the `daemons` argument, unless
  ## there are daemons already running
  mirai::daemons_set() || {
    on.exit(mirai::daemons(0L), add = TRUE)
    mirai::daemons(n = daemons)
  }

  ## ensure that `what` and `args` have names assigned
  if (!is.null(what))
    names(what) <- get_element_names(what.quo, use_names = TRUE)
  if (!is.null(args)) {
    names(args) <- get_element_names(args.quo, use_names = FALSE)
  }

  ## expand the set of inputs with their listified version
  if (listify_what)
    what <- append_listified(what)

  ## expand the set of inputs according to the arguments provided
  what <- modify_args(what, args)

  ## join all regular expression patterns
  joined_patterns <- paste0(c(ignore_patterns,
                              "is missing, with no default"),
                            collapse = "|")
  joined_patterns <- gsub("^\\|", "", joined_patterns) # remove extra |

  ## start fuzzing
  cli::cli_alert_info(c("Fuzzing {length(funs)} function{?s} ",
                        "with {length(what)} input{?s} ",
                        "(using {mirai::info()[['connections']]} daemon{?s})"))
  if (is.null(package))
    cli::cli_alert_info(c("Functions will be searched in the global namespace ",
                          "as {.arg package} was not specified"))

  ## ensure that we always show some progress
  if (is.null(getOption("cli.progress_show_after"))) {
    opt <- options(cli.progress_show_after = 0.1)
    on.exit(options(opt), add = TRUE)
  }

  ## create the queue
  queue <- setup_queue(funs, what, timeout = timeout,
                       package, joined_patterns, ignore_warnings)

  ## fuzz the functions asynchronously
  runs <- queue$process()

  ## returned object
  structure(list(runs = runs,
                 funs = funs,
                 package = package %||% NA,
                 ignore_patterns = ignore_patterns,
                 ignore_warnings = ignore_warnings),
            class = "cbtf")
}

#' Apply additional whitelist patterns to the results of a fuzz run
#'
#' This allows for post-hoc whitelisting of results according to the patterns
#' specified.
#'
#' @param object An object of class `cbtf`.
#' @param patterns One or more strings containing regular expressions to
#'        match the errors to whitelist.
#'
#' @return
#' An object of class `cbtf` with the additional whitelist patterns applied.
#'
#' @examples
#'
#' ## this reports a false positive result
#' (res <- fuzz(funs = "matrix", what = test_inputs("scalar")))
#'
#' ## with whitelisting, we can remove that
#' whitelist(res, "must be of a vector type")
#'
#' @seealso [fuzz]
#'
#' @export
whitelist <- function(object, patterns) {
  from <- "whitelist"
  validate_class(object, "cbtf", from = from)
  validate_class(patterns, "character", from = from, remove_empty = TRUE)

  ## join all regular expression patterns
  joined_patterns <- paste0(patterns, collapse = "|")
  joined_patterns <- gsub("^\\|", "", joined_patterns) # remove extra |

  ## apply the new whitelist patterns to errors and warnings
  object$runs <- lapply(object$runs, function(x) {
    x$res[grepl(joined_patterns, x$msg) & x$msg != "SKIP"] <- "OK"
    x
  })
  object$ignore_patterns <- setdiff(c(object$ignore_patterns, patterns), "")
  object
}
