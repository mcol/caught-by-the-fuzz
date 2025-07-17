test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(fuzz(NA, NULL),
               "'funs' should be of class character")
  expect_error(fuzz(NULL, NULL),
               "'funs' should be of class character")
  expect_error(fuzz(what = NA),
               "'funs' should be of class character")
  expect_error(fuzz("list"),
               "'what' must be specified")
})

test_that("check skipped functions", {
  testthat::skip_on_cran()

  SW({
  expect_skip_reason(fuzz("list", NULL),
                     "Doesn't accept arguments")
  expect_skip_reason(fuzz("iris", NULL),
                     "Not a function")
  expect_skip_reason(fuzz(".not.found.", NULL),
                     "Object not found in the global namespace")
  expect_skip_reason(fuzz(".not.found.", NULL, package = "CBTF"),
                     "Object not found in the 'CBTF' namespace")

  ## function with no arguments
  expect_skip_reason(fuzz("Sys.Date", NULL),
                     "Doesn't accept arguments")

  ## must use `assign` otherwise the name cannot be found by the `get` call
  assign("with.readline", function(val) readline("Prompt"), envir = .GlobalEnv)
  expect_skip_reason(fuzz("with.readline", NULL),
                     "Contains readline()")
  rm("with.readline", envir = .GlobalEnv)
  })
})

test_that("check object returned", {
  testthat::skip_on_cran()

  funs <- c("list", "data.frame")
  SW({
  res <- fuzz(funs, NULL)
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
  expect_equal(res$package,
               NA)

  ## check that we store the package attribute correctly
  funs <- structure(c("list", "data.frame"), package = "packagename")
  SW({
  res <- fuzz(funs, NULL)
  })
  expect_equal(res$package,
               "packagename")
})

test_that("check classes returned", {
  testthat::skip_on_cran()

  SW({
  expect_what(fuzz("list", NULL),
              "NULL")
  expect_what(fuzz("list", NA),
              "NA")
  expect_what(fuzz("list", data.frame()),
              "data.frame()")
  expect_what(fuzz("list", list()),
              "list()")
  expect_what(fuzz("list", 1:3),
              "1:3")
  expect_what(fuzz("list", letters),
              "letters")

  ## TODO: this seems unfortunate, could it be made to return `letters`?
  what <- letters
  expect_what(fuzz("list", what),
              "what")
  })
})

test_that("self fuzz", {
  testthat::skip_on_cran()

  SW({
  expect_message(expect_output(print(fuzz("fuzz", list())),
                               "OK 1"),
                 "You didn't get caught by the fuzz!")
  expect_message(expect_output(print(fuzz("fuzz", NULL)),
                               "OK 1"),
                 "You didn't get caught by the fuzz!")
  })
})
