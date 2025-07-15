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

test_that("check functionality", {
  testthat::skip_on_cran()

  SW({
  expect_skip_reason(fuzz("list", NULL),
                     "Doesn't accept arguments")
  expect_skip_reason(fuzz("iris", NULL),
                     "Not a function")
  expect_skip_reason(fuzz(".not.found.", NULL),
                     "Object not found")

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
  expect_output(print(fuzz("fuzz", list())),
                "'funs' should be of class character")
  expect_output(print(fuzz("fuzz", NULL)),
                "'funs' should be of class character")
  })
})
