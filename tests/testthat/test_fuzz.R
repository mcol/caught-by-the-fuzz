test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(fuzz(NA, NULL),
               "'funs' should be of class character")
  expect_error(fuzz(NULL, NULL),
               "'funs' should be of class character")
  expect_error(fuzz(what = NA),
               "'funs' should be of class character")
  expect_error(fuzz(character(0), NULL),
               "'funs' is empty")
  expect_error(fuzz("list"),
               "'what' should be of class list")
  expect_error(fuzz("list", list()),
               "'what' is empty")
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
  res <- fuzz(funs, list(NULL))
  })
  expect_s3_class(res,
                  "cbtf")
  expect_named(res,
               c("runs", "package"))
  expect_length(res$runs,
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
})

test_that("check classes returned", {
  testthat::skip_on_cran()

  SW({
  expect_what(fuzz("list", alist(NULL)),
              "NULL")
  expect_what(fuzz("list", alist(NA)),
              "NA")
  expect_what(fuzz("list", alist(data.frame())),
              "data.frame()")
  expect_what(fuzz("list", alist(list())),
              "list()")
  expect_what(fuzz("list", alist(1:3)),
              "1:3")
  expect_what(fuzz("list", alist(letters)),
              "letters")

  ## TODO: this seems unfortunate, could it be made to return `letters`?
  what <- letters
  expect_what(fuzz("list", alist(what)),
              "what")
  })
})

test_that("fuzzer", {
  testthat::skip_on_cran()

  res <- fuzzer("list", NULL)
  expect_s3_class(res,
                  "data.frame")
  expect_equal(attr(res, "what"),
               "")
  res <- fuzzer("list", NULL, what.char = "NA")
  expect_equal(attr(res, "what"),
               "NA")
})

test_that("self fuzz", {
  testthat::skip_on_cran()

  SW({
  expect_message(expect_output(print(fuzz("fuzz", list(list()))),
                               "OK 1"),
                 "You didn't get caught by the fuzz!")
  expect_message(expect_output(print(fuzz("fuzz", list(NULL))),
                               "OK 1"),
                 "You didn't get caught by the fuzz!")
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
               "'package' is empty")
  expect_error(get_exported_functions("CBTF", NA),
               "'ignore.names' should be of class character")
  expect_error(get_exported_functions("CBTF", character(0)),
               "'ignore.names' is empty")

  funs <- get_exported_functions("CBTF")
  expect_type(funs,
              "character")
  expect_equal(as.character(funs),
               c("fuzz", "get_exported_functions"))
  expect_equal(attr(funs, "package"),
               "CBTF")
})
