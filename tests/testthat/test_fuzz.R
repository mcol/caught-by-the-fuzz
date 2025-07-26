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
  assign("with.readline", function(val) readline("Prompt"), envir = .GlobalEnv)
  expect_skip_reason(fuzz("with.readline", list(NULL)),
                     "Contains readline()")
  rm("with.readline", envir = .GlobalEnv)
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
               c("runs", "package"))
  expect_length(res,
                1)
  expect_s3_class(res$runs[[1]],
                  "data.frame")
  expect_named(res$runs[[1]],
               c("fun", "res", "msg"))
  expect_equal(nrow(res$runs[[1]]),
               length(funs))
  expect_equal(res$runs[[1]]$res,
               c("OK", "OK", "OK"))
  expect_equal(res$package,
               NA)

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
                length(input_list))
  expect_equal(res$package,
               NA)
  SW({
  res <- fuzz("list", listify_what = TRUE)
  })
  expect_length(res,
                length(input_list) * 2)
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

test_that("fuzzer", {
  testthat::skip_on_cran()

  res <- fuzzer("list", NULL)
  expect_s3_class(res,
                  "data.frame")
  expect_equal(res$res,
               "OK")
  expect_equal(attr(res, "what"),
               "")

  res <- fuzzer("list", NULL, what_char = "NA")
  expect_equal(attr(res, "what"),
               "NA")

  res <- fuzzer("ls", NA)
  expect_equal(res$res,
               "FAIL")
  expect_equal(res$msg,
               "invalid object for 'as.environment'")
  res <- fuzzer("median", letters)
  expect_equal(res$res,
               "WARN")
  expect_equal(res$msg,
               "argument is not numeric or logical: returning NA")

  ## this passes because the warning message contains "mean.default"
  res <- fuzzer("mean", letters)
  expect_equal(res$res,
               "OK")
  expect_equal(res$msg,
               "")
})

test_that("self fuzz", {
  testthat::skip_on_cran()

  SW({
  expect_output(expect_pass_message(fuzz("fuzz", list(list()))),
                "OK 1")
  expect_output(expect_pass_message(fuzz("fuzz", list(NULL))),
                "OK 1")

  ## fuzz test other arguments by currying the function
  curry_fuzz_for <- function(argname) {
    function(arg) do.call(fuzz, list("list", setNames(list(arg), argname)))
  }

  withr::with_envvar(c(package_arg = curry_fuzz_for("package")),
                     expect_pass_message(fuzz("package_arg")))
  withr::with_envvar(c(listify_arg = curry_fuzz_for("listify_what")),
                     expect_pass_message(fuzz("listify_arg")))
  withr::with_envvar(c(patterns_arg = curry_fuzz_for("ignore_patterns")),
                     expect_pass_message(fuzz("patterns_arg")))
  withr::with_envvar(c(warnings_arg = curry_fuzz_for("ignore_warnings")),
                     expect_pass_message(fuzz("warnings_arg")))

  ## as `what` expects a list argument, we can't use curry_fuzz_for()
  withr::with_envvar(c(what_arg = function(arg) fuzz("list", what = list(arg))),
                     expect_pass_message(fuzz("what_arg")))
  })
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
  expect_error(get_exported_functions("CBTF", NA),
               "'ignore_names' should be of class character")
  expect_error(get_exported_functions("CBTF", character(0)),
               "'ignore_names' is an empty character")

  funs <- get_exported_functions("CBTF")
  expect_type(funs,
              "character")
  expect_equal(as.character(funs),
               c("fuzz", "get_exported_functions"))
  expect_equal(attr(funs, "package"),
               "CBTF")

  SW({
  expect_pass_message(fuzz("get_exported_functions"))
  withr::with_envvar(c(package_arg = function(arg) get_exported_functions("CBTF", package = arg)),
                     expect_pass_message(fuzz("package_arg")))
  })

  ## tested with mime 0.13
  skip_if_not_installed("mime")
  funs <- get_exported_functions("mime")
  expect_equal(as.character(funs),
               c("guess_type", "parse_multipart"))
})
