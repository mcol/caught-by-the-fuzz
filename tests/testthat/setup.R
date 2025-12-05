## set up local daemons
daemons(2)

## silence output and warnings
SW <- function(expr) capture.output(suppressMessages(suppressWarnings(expr)))

## check that the `res` and `msg` fields match the expectation
expect_fuzz_result <- function(res, exp_res, exp_msg) {
  expect_equal(sapply(res$runs, function(x) x$res),
               exp_res)
  expect_equal(sapply(res$runs, function(x) x$msg),
               exp_msg)
}

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
expect_pass_message <- function(res, msg = "") {
  expect_message(print(res),
                 "You didn't get caught by the fuzz!")
  expect_output(print(res),
                "FAIL 0 | WARN 0 | SKIP 0", fixed = TRUE)
  expect_equal(res$runs[[1]]$msg, msg)
}

## check that fuzzing found errors
expect_fail_message <- function(res) {
  expect_message(print(res),
                 "CAUGHT BY THE FUZZ!")
  expect_output(print(res),
                "SKIP 0 | OK 0", fixed = TRUE)
}
