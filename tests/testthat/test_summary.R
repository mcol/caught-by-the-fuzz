test_that("print", {
  testthat::skip_on_cran()

  SW({
  expect_output(expect_message(
      print(fuzz("list", list(NA, c(1, 2, 3)))),
      "You didn't get caught by the fuzz!"),
      "OK 2")
  expect_output(expect_message(
      print(fuzz("ls", list(NA, c(1, 2, 3)))),
      "CAUGHT BY THE FUZZ!"),
      "FAIL 2")
  })
})
