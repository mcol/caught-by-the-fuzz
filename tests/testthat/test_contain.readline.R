test_that("real calls to readline() are caught", {
  testthat::skip_on_cran()

  fun1 <- function() readline("Prompt")
  fun2 <- function() input <- readline("Prompt")
  fun3 <- function() if (TRUE) input <- readline("Prompt")
  fun4 <- function() if (TRUE) while(TRUE) input <- readline("Prompt")

  expect_true(contains.readline(fun1))
  expect_true(contains.readline(fun2))
  expect_true(contains.readline(fun3))
  expect_true(contains.readline(fun4))
})

test_that("other uses of readline() are ignored", {
  testthat::skip_on_cran()

  fun1 <- function() print("")
  fun2 <- function() "readline()"
  fun3 <- function() print("readline()")

  expect_false(contains.readline(fun1))
  expect_false(contains.readline(fun2))
  expect_false(contains.readline(fun3))
})
