pass.msg <- "You didn't get caught by the fuzz"

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

  expect_output(fuzz("list", NULL),
                pass.msg)
  expect_output(fuzz("iris", NULL),
                pass.msg)

  ## must use `assign` otherwise the name cannot be found by the `get` call
  assign("with.readline", function() readline("Prompt"), envir = .GlobalEnv)
  expect_output(fuzz("with.readline", NULL),
                pass.msg)
  rm("with.readline", envir = .GlobalEnv)
})
