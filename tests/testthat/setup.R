## silence output and warnings
SW <- function(expr) capture.output(suppressMessages(suppressWarnings(expr)))

## check that the `what` attribute matches the expectation
expect_what <- function(res, exp) {
  expect_equal(attr(res, "what"), exp)
}

## check that the function is skipped for the correct reason
expect_skip_reason <- function(res, reason) {
  expect_equal(paste(res[[1]]$res, res[[1]]$msg),
               paste("SKIP", reason))
}
