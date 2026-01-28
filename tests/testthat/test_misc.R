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
               "'iris' should be a single data.frame value")
  expect_error(validate_class(letters, "character", scalar = TRUE),
               "'letters' should be a single character value")
  expect_error(validate_class(letters, c("numeric", "character"), scalar = TRUE),
               "^\\[fuzz\\] 'letters' should be a single character value$")
  arg <- "Inf"
  expect_error(validate_class(arg, "numeric"),
               "'arg' should be of class numeric")
  arg <- NULL
  expect_error(validate_class(arg, "character"),
               "'arg' should be of class character")
  arg <- ""
  expect_error(validate_class(arg, "character", remove_empty = TRUE),
               "'arg' is an empty character")
  arg <- list()
  expect_error(validate_class(arg, "list", remove_empty = TRUE),
               "'arg' is an empty list")
  arg <- list("")
  expect_error(validate_class(arg, "list", remove_empty = TRUE),
               "'arg' is an empty list")
  arg <- 1
  expect_error(validate_class(arg, "numeric", min = 2),
               "'arg' should be at least 2")
  arg <- numeric()
  expect_error(validate_class(arg, "numeric", min = 2),
               "'arg' should be at least 2")

  expect_silent(validate_class(iris, "data.frame"))
  expect_silent(validate_class(NULL, "data.frame", null.ok = TRUE))
  expect_silent(validate_class(iris, "data.frame", remove_empty = TRUE))
  expect_silent(validate_class("", "character"))
  expect_silent(validate_class(list(""), "list"))
  expect_silent(validate_class(2, "numeric", min = 1))
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
  expect_equal(check_fuzzable(":", NULL),
               "Doesn't specify number of arguments")
  expect_equal(check_fuzzable("Sys.Date", NULL),
               "Doesn't accept arguments")
  expect_equal(check_fuzzable("+", NULL, num_args = 3),
               "Accepts only up to 2 arguments")

  ## check that the ... is interpreted correctly
  expect_equal(check_fuzzable("list", NULL, num_args = 3),
               list)

  ## must use `assign` otherwise the name cannot be found by the `get` call
  assign(".with.readline.", function(val) readline("Test"), envir = .GlobalEnv)
  expect_equal(check_fuzzable(".with.readline.", NULL),
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

test_that("get_element_names", {
  testthat::skip_on_cran()

  expect_equal(get_element_names(rlang::quo(list(NA, NULL)),
                                 use_names = TRUE),
               c("NA", "NULL"))
  expect_equal(get_element_names(rlang::quo(list(first = NA, NULL)),
                                 use_names = TRUE),
               c("first", "NULL"))
  expect_equal(get_element_names(rlang::quo(list(first = NA, iris)),
                                 use_names = TRUE),
               c("first", "iris"))
  expect_equal(get_element_names(rlang::quo(list(df = iris, iris)),
                                 use_names = TRUE),
               c("df", "iris"))

  ## list passed indirectly
  what <- list(iris, data.frame())
  expect_equal(get_element_names(rlang::quo(what),
                                 use_names = TRUE),
               c("structure(list(Sepal.Length = c(5.1, 4.9, 4.7, 4.6, 5, 5.4, 4.6, ",
                 "structure(list(), names = character(0), row.names = integer(0), class = \"data.frame\")"))

  what <- list(first = iris, data.frame())
  expect_equal(get_element_names(rlang::quo(what),
                                 use_names = TRUE),
               c("first",
                 "structure(list(), names = character(0), row.names = integer(0), class = \"data.frame\")"))
  expect_equal(get_element_names(rlang::quo(test_inputs("na")),
                                 use_names = TRUE),
               c("NA_real_", "NA_integer_", "NA_character_", "NA"))
})

test_that("modify_args", {
  testthat::skip_on_cran()

  expect_equal(modify_args(namify(1, 2, 3), args = NULL),
               lapply(namify(1, 2, 3), list))
  expect_equal(modify_args(what = NULL, args = list(1, 2, 3)),
               structure(list(list(1, 2, 3)),
                         names = "1, 2, 3"))
  expect_equal(modify_args(what = NULL, args = namify(1, 2, 3)),
               structure(list(list(1, 2, 3)),
                         names = "1, 2, 3"))
  expect_equal(modify_args(namify(NA, 1.2), args = namify(TRUE, 2:5)),
               structure(list(list(NA, 2:5), list(1.2, 2:5),
                              list(TRUE, NA), list(TRUE, 1.2)),
                         names = c("NA, 2:5", "1.2, 2:5",
                                   "TRUE, NA", "TRUE, 1.2")))

  ## unique
  expect_equal(modify_args(namify(1, 2, 3), args = namify(1, 2)),
               structure(list(list(1, 2), list(2, 2), list(3, 2),
                              list(1, 1), list(1, 3)),
                         names = c("1, 2", "2, 2", "3, 2",
                                   "1, 1", "1, 3")))

  ## NULL
  expect_equal(modify_args(namify(0, NULL), args = namify(1, 2)),
               structure(list(list(0, 2), list(NULL, 2),
                              list(1, 0), list(1, NULL)),
                         names = c("0, 2", "NULL, 2",
                                   "1, 0", "1, NULL")))
})
