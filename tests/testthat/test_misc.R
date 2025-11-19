test_that("validate_class", {
  testthat::skip_on_cran()

  arg <- data.frame()
  expect_error(validate_class(arg, "character"),
               "[fuzz] 'arg' should be of class character",
               fixed = TRUE)
  expect_error(validate_class(arg, "character", from = "function_name"),
               "[function_name] 'arg' should be of class character",
               fixed = TRUE)
  expect_error(validate_class(arg, "data.frame"),
               "'arg' is an empty data.frame")
  expect_error(validate_class(iris, "data.frame", scalar = TRUE),
               "'iris' should be a data.frame scalar")
  expect_error(validate_class(letters, "character", scalar = TRUE),
               "'letters' should be a character scalar")
  arg <- ""
  expect_error(validate_class(arg, "character", remove_empty = TRUE),
               "'arg' is an empty character")
  arg <- list()
  expect_error(validate_class(arg, "list", remove_empty = TRUE),
               "'arg' is an empty list")
  arg <- list("")
  expect_error(validate_class(arg, "list", remove_empty = TRUE),
               "'arg' is an empty list")

  expect_silent(validate_class(iris, "data.frame"))
  expect_silent(validate_class(iris, "data.frame", remove_empty = TRUE))
  expect_silent(validate_class("", "character"))
  expect_silent(validate_class(list(""), "list"))
})

test_that("fuzz_error", {
  testthat::skip_on_cran()

  expect_error(fuzz_error("message"),
               "[fuzz] message", fixed = TRUE)
  expect_error(fuzz_error("part 1", "part 2", from = "function_name"),
               "[function_name] part 1 part 2", fixed = TRUE)
})

test_that("check_fuzzable", {
  testthat::skip_on_cran()

  expect_equal(check_fuzzable(".not.existing.", NULL),
               "Object not found in the global namespace")
  expect_equal(check_fuzzable(".not.existing.", "package_name"),
               "Object not found in the 'package_name' namespace")
  expect_equal(check_fuzzable(".Device", NULL),
               "Not a function")
  expect_equal(check_fuzzable("Sys.Date", NULL),
               "Doesn't accept arguments")

  ## must use `assign` otherwise the name cannot be found by the `get` call
  assign(".with.readline.", function(val) readline("Test"), envir = .GlobalEnv)
  expect_equal(check_fuzzable(".with.readline.", NULL),
               "Contains readline()")
  expect_equal(check_fuzzable(".with.readline.", NULL, skip_readline = FALSE),
               .with.readline.)
  rm(".with.readline.", envir = .GlobalEnv)

  expect_true(is.function(check_fuzzable("list", NULL)))
  expect_true(is.function(check_fuzzable("mean", NULL)))

  assign(".deprecated.", envir = .GlobalEnv,
         function(arg) .Deprecated("new"))
  expect_true(is.function(check_fuzzable(".deprecated.", NULL,
                                         ignore_deprecated = FALSE)))
  expect_equal(check_fuzzable(".deprecated.", NULL, ignore_deprecated = TRUE),
               "Deprecated function")
  rm(".deprecated.", envir = .GlobalEnv)
})

test_that("tocolour", {
  testthat::skip_on_cran()

  expect_equal(tocolour("FAIL"),
               as.character(cli::col_yellow("FAIL")))
  expect_equal(tocolour("WARN"),
               as.character(cli::col_magenta("WARN")))
  expect_equal(tocolour("SKIP"),
               as.character(cli::col_blue("SKIP")))
  expect_equal(tocolour("OK"),
               as.character(cli::col_green("OK")))
  expect_equal(tocolour("FAIL", 0),
               "FAIL 0")
  expect_equal(tocolour("FAIL", 1),
               paste(cli::col_yellow("FAIL"), 1))
  expect_equal(tocolour("FAIL", 1, FALSE),
               "FAIL 1")
})

test_that("contains_readline", {
  testthat::skip_on_cran()

  ## check that real calls to readline() are caught
  fun1 <- function() readline("Prompt")
  fun2 <- function() input <- readline("Prompt")
  fun3 <- function() if (TRUE) input <- readline("Prompt")
  fun4 <- function() if (TRUE) while (TRUE) input <- readline("Prompt")

  expect_true(contains_readline(fun1))
  expect_true(contains_readline(fun2))
  expect_true(contains_readline(fun3))
  expect_true(contains_readline(fun4))

  ## check that other uses of readline() are ignored
  fun1 <- function() print("")
  fun2 <- function() "readline()"
  fun3 <- function() print("readline()")

  expect_false(contains_readline(fun1))
  expect_false(contains_readline(fun2))
  expect_false(contains_readline(fun3))
})

test_that("namify", {
  testthat::skip_on_cran()

  expect_equal(namify(NA),
               list("NA" = NA))
  expect_equal(namify(12, one = 1, "", iris, matrix(1, 0, 1)),
               list("12" = 12,
                    "one" = 1,
                    "\"\"" = "",
                    "iris" = iris,
                    "matrix(1, 0, 1)" = matrix(1, 0, 1)))
})

test_that("append_listified", {
  testthat::skip_on_cran()

  ## list
  expect_equal(append_listified(list(NA)),
               list(NA, list(NA)))
  expect_equal(append_listified(list(list())),
               list(list(), list(list())))
  res <- append_listified(list(matrix = matrix()))
  expect_equal(res,
               list(matrix = matrix(), "list(matrix)" = list(matrix())))
  res <- append_listified(list(`NULL` = NULL, `NA` = NA, `0L` = 0L))
  expect_named(res,
               c("NULL", "NA", "0L", "list(NULL)", "list(NA)", "list(0L)"))
  res <- append_listified(list(m0 = matrix(0, 0, 0), m1 = matrix(1, 0, 1)))
  expect_named(res,
               c("m0", "m1", "list(m0)", "list(m1)"))

  ## alist
  expect_equal(append_listified(alist(NA)),
               list(NA, list(NA)))
  expect_equal(append_listified(alist(list())),
               list(quote(list()), list(quote(list()))))
})
