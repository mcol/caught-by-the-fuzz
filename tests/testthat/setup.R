## silence output and warnings
SW <- function(expr) capture.output(suppressMessages(suppressWarnings(expr)))

## check that the `what` attribute matches the expectation
expect_what <- function(res, exp) {
  expect_equal(sapply(res$runs, function(x) attr(x, "what")),
               exp)
}

## check that the function is skipped for the correct reason
expect_skip_reason <- function(res, reason) {
  expect_equal(paste(res$runs[[1]]$res, res$runs[[1]]$msg),
               paste("SKIP", reason))
}

## check that fuzzing found no errors
expect_pass_message <- function(res) {
  expect_message(print(res),
                 "You didn't get caught by the fuzz!")
}

## check that fuzzing found errors
expect_fail_message <- function(res) {
  expect_message(print(res),
                 "CAUGHT BY THE FUZZ!")
}
