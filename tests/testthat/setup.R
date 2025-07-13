## silence output and warnings
SW <- function(expr) capture.output(suppressMessages(suppressWarnings(expr)))

## check that the `what` attribute matches the expectation
expect_what <- function(res, exp) {
  expect_equal(attr(res, "what"), exp)
}
