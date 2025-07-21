test_that("fuzz_error", {
  testthat::skip_on_cran()

  expect_error(fuzz_error("message"),
               "[fuzz] message", fixed = TRUE)
  expect_error(fuzz_error("part 1", "part 2"),
               "[fuzz] part 1 part 2", fixed = TRUE)
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
  fun4 <- function() if (TRUE) while(TRUE) input <- readline("Prompt")

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
