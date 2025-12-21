## remove local binding generated further down with `assign()`
withr::defer(rm(".local_fun.", envir = .GlobalEnv))

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(fuzz(NA, NULL),
               "'funs' should be of class character")
  expect_error(fuzz(NULL, NULL),
               "'funs' should be of class character")
  expect_error(fuzz(what = NA),
               "'funs' should be of class character")
  expect_error(fuzz(character(0), NULL),
               "'funs' is an empty character")
  expect_error(fuzz("list", list()),
               "'what' is an empty list")
  expect_error(fuzz("list", package = letters),
               "'package' should be a character scalar")
  expect_error(fuzz("list", package = ""),
               "'package' is an empty character")
  expect_error(fuzz("list", listify_what = NULL),
               "'listify_what' should be of class logical")
  expect_error(fuzz("list", listify_what = c(TRUE, FALSE)),
               "'listify_what' should be a logical scalar")
  expect_error(fuzz("list", ignore_patterns = TRUE),
               "'ignore_patterns' should be of class character")
  expect_error(fuzz("list", list(NA), ignore_warnings = NA),
               "'ignore_warnings' should be of class logical")
  expect_error(fuzz("list", list(NA), ignore_warnings = c(TRUE, FALSE)),
               "'ignore_warnings' should be a logical scalar")
  expect_error(fuzz("list", list(NA), daemons = NA_integer_),
               "'daemons' should be of class integer, numeric")
  expect_error(fuzz("list", list(NA), daemons = c(2, 3)),
               "'daemons' should be a numeric scalar")
  expect_error(fuzz("list", list(NA), daemons = 0),
               "'daemons' should be at least 1")
})

test_that("check skipped functions", {
  testthat::skip_on_cran()

  SW({
  expect_skip_reason(fuzz("Sys.Date", list(NULL)),
                     "Doesn't accept arguments")
  expect_skip_reason(fuzz("iris", list(NULL)),
                     "Not a function")
  expect_skip_reason(fuzz(".not.found.", list(NULL)),
                     "Object not found in the global namespace")
  expect_skip_reason(fuzz(".not.found.", list(NULL), package = "CBTF"),
                     "Object not found in the 'CBTF' namespace")

  ## must use `assign` otherwise the name cannot be found by the `get` call
  assign(".local_fun.", envir = .GlobalEnv,
         function(val) readline("Test"))
  expect_pass_message(fuzz(".local_fun.", list(NULL)))
  })
})

test_that("check object returned", {
  testthat::skip_on_cran()

  funs <- c("list", "data.frame", "+")
  SW({
  expect_message(res <- fuzz(funs, list(NULL)),
                 "Functions will be searched in the global namespace")
  })
  expect_s3_class(res,
                  "cbtf")
  expect_named(res,
               c("runs", "funs", "package", "ignore_patterns", "ignore_warnings"))
  expect_length(res,
                length(funs))
  expect_s3_class(res[[1]],
                  "data.frame")
  expect_named(res$runs[[1]],
               c("res", "msg"))
  expect_equal(nrow(res[[1]]),
               length(funs))
  expect_equal(res$runs[[1]]$res,
               c("OK", "OK", "OK"))
  expect_equal(res$runs[[1]]$msg,
               c("", "", "invalid argument to unary operator"))
  expect_equal(res$package,
               NA)

  SW({
  res <- fuzz("ls", list(NA))
  })
  expect_fuzz_result(res,
                     "FAIL", "invalid object for 'as.environment'")

  SW({
  res <- fuzz("median", list(letters))
  })
  expect_fuzz_result(res,
                     "WARN", "argument is not numeric or logical: returning NA")

  ## ignore warnings
  SW({
  res <- fuzz("ls", list(NA), ignore_warnings = TRUE)
  })
  expect_fuzz_result(res,
                     "FAIL", "invalid object for 'as.environment'")

  SW({
  res <- fuzz("median", list(letters), ignore_warnings = TRUE)
  })
  expect_fuzz_result(res,
                     "OK", "argument is not numeric or logical: returning NA")

  ## timeout
  assign(".local_fun.", envir = .GlobalEnv,
         function(arg) Sys.sleep(10))
  SW({
  res <- fuzz(".local_fun.", list(NA))
  })
  expect_fuzz_result(res,
                     "OK", "Timed out after 2 seconds")

  ## in case of both error and warning, we should report the error
  SW({
  assign(".local_fun.", envir = .GlobalEnv,
         function(arg) {
           warning("a warning")
           stop("an error", call. = FALSE)
         })
  res <- fuzz(".local_fun.", list(NA))
  })
  expect_fuzz_result(res,
                     "FAIL", "an error")

  ## check that we store the package attribute correctly
  funs <- structure(c("list", "data.frame"), package = "packagename")
  SW({
  res <- fuzz(funs, list(NULL))
  })
  expect_equal(res$package,
               "packagename")

  ## test with the default inputs
  SW({
  res <- fuzz("list")
  })
  expect_length(res,
                length(test_inputs()))
  expect_equal(res$package,
               NA)
  SW({
  res <- fuzz("list", listify_what = TRUE)
  })
  expect_length(res,
                length(test_inputs()) * 2)
})

test_that("check classes returned", {
  testthat::skip_on_cran()

  SW({
  expect_what(fuzz("list", list(NULL)),
              "NULL")
  expect_what(fuzz("list", list(NA)),
              "NA")
  expect_what(fuzz("list", list(`data.frame()` = data.frame())),
              "data.frame()")
  expect_what(fuzz("list", list(list())),
              "list()")
  expect_what(fuzz("list", list(1:3)),
              "1:3")
  expect_what(fuzz("list", list(letters = letters)),
              "letters")
  expect_what(fuzz("list", list(NA, letters = letters)),
              c("NA", "letters"))
  what <- letters
  expect_what(fuzz("list", list(input = what)),
              "input")
  })
})

test_that("self fuzz", {
  testthat::skip_on_cran()

  ## start nested daemons
  mirai::everywhere(mirai::daemons(1, dispatcher = FALSE))

  SW({
  expect_output(expect_pass_message(fuzz("fuzz", list(list())),
                                    "[fuzz] 'funs' should be of class character"),
                "OK 1")
  expect_output(expect_pass_message(fuzz("fuzz", list(NULL)),
                                    "[fuzz] 'funs' should be of class character"),
                "OK 1")

  ## fuzz test other arguments by currying the function
  curry_fuzz_for <- function(argname) {
    function(arg) do.call(fuzz, setNames(list("list", arg), c("funs", argname)))
  }
  test_self_fuzz <- function(argname, argtype) {
    assign(".local_fun.", envir = .GlobalEnv,
           curry_fuzz_for(argname))
    expect_pass_message(fuzz(".local_fun.",
                             ignore_patterns = "\\[fuzz\\]"),
                        sprintf("[fuzz] '%s' should be of class %s",
                                argname, paste(argtype, collapse = ", ")))
  }

  test_self_fuzz("package", "character")
  test_self_fuzz("listify_what", "logical")
  test_self_fuzz("ignore_patterns", "character")
  test_self_fuzz("ignore_warnings", "logical")
  test_self_fuzz("daemons", c("integer", "numeric"))

  ## as `what` expects a list argument, we can't use curry_fuzz_for()
  assign(".local_fun.", envir = .GlobalEnv,
         function(arg) fuzz("list", what = list(arg)))
  expect_pass_message(fuzz(".local_fun."))
  })

  ## shut down nested daemons
  mirai::everywhere(mirai::daemons(0))
})

test_that("whitelist", {
  testthat::skip_on_cran()

  SW({
  res <- fuzz("numToInts")
  })
  expect_error(whitelist(NA, NA),
               "[whitelist] 'object' should be of class cbtf",
               fixed = TRUE)
  expect_error(whitelist(res, NA),
               "'patterns' should be of class character")
  expect_error(whitelist(res, ""),
               "'patterns' is an empty character")

  ignore_patterns <- c("cannot be coerced to type",
                       "NAs introduced by coercion")
  res.new <- whitelist(res, c("", ignore_patterns))
  SW({
  expect_pass_message(res.new)
  })
  expect_equal(res.new$ignore_patterns,
               ignore_patterns)
})

test_that("get_exported_functions", {
  testthat::skip_on_cran()

  expect_error(get_exported_functions(),
               "[get_exported_functions] 'package' should be of class character",
               fixed = TRUE)
  expect_error(get_exported_functions(NA),
               "'package' should be of class character")
  expect_error(get_exported_functions(character(0)),
               "'package' is an empty character")
  expect_error(get_exported_functions("like this"),
               "there is no package called 'like this'")
  expect_error(get_exported_functions("CBTF", NA),
               "'ignore_names' should be of class character")
  expect_error(get_exported_functions("CBTF", character(0)),
               "'ignore_names' is an empty character")
  expect_error(get_exported_functions("CBTF", "ignore", NA),
               "'ignore_deprecated' should be of class logical")
  expect_error(get_exported_functions("CBTF", "ignore", c(TRUE, FALSE)),
               "'ignore_deprecated' should be a logical scalar")

  funs <- get_exported_functions("CBTF")
  expect_type(funs,
              "character")
  expect_equal(as.character(funs),
               c("fuzz",
                 "get_exported_functions",
                 "namify",
                 "test_inputs",
                 "whitelist"))
  expect_equal(attr(funs, "package"),
               "CBTF")

  funs <- get_exported_functions("base")
  expect_false(".Device" %in% funs)
  expect_false("Sys.Date" %in% funs)

  SW({
  expect_pass_message(fuzz("get_exported_functions"),
                      "[get_exported_functions] 'package' should be of class character")
  assign(".local_fun.", envir = .GlobalEnv,
         function(arg) get_exported_functions(package = arg))
  expect_pass_message(fuzz(".local_fun.",
                           ignore_patterns = "\\[get_exported_functions\\]"),
                      "[get_exported_functions] 'package' should be of class character")
  })

  ## tested with mime 0.13
  skip_if_not_installed("mime")
  funs <- get_exported_functions("mime")
  expect_equal(as.character(funs),
               c("guess_type", "parse_multipart"))
})
