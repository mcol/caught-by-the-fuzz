test_that("test_inputs", {
  testthat::skip_on_cran()

  expect_error(test_inputs(NA),
               "'use' should be of class character")
  expect_error(test_inputs(""),
               "'use' is an empty character")
  expect_error(test_inputs("error"),
               "[test_inputs] No valid tests selected, valid names are: 'all'",
               fixed = TRUE)
  expect_error(test_inputs(skip = NA),
               "'skip' should be of class character")
  expect_error(test_inputs("scalar", skip = "scalar"),
               "No valid tests selected, valid names are: 'all', 'scalar'")

  expect_equal(test_inputs(use = "scalar", skip = "nonexisting"),
               test_inputs(use = "scalar"))
  expect_equal(test_inputs(use = c("scalar", "scalar")),
               test_inputs(use = "scalar"))
  expect_equal(test_inputs(use = c("all", "scalar")),
               test_inputs())
  expect_equal(test_inputs(c("all", "error")),
               test_inputs())
  expect_equal(test_inputs("help"),
               c("all", "scalar", "numeric", "integer", "logical",
                 "character", "factor", "data.frame", "matrix", "array",
                 "date", "raw", "na", "list"))
  expect_equal(test_inputs("help"),
               test_inputs(c("scalar", "help")))
})
