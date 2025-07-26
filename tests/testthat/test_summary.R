test_that("summary", {
  testthat::skip_on_cran()

  SW({
  res <- fuzz("list", list(NA, c(1, 2, 3)))
  expect_message(sum <- summary(res),
                 "Fuzzed 1 function on 2 inputs:")
  expect_s3_class(sum,
                  "data.frame")
  expect_named(sum,
               c("fun", "what", "res", "msg"))
  expect_equal(sum$what,
               c("NA", "c(1, 2, 3)"))
  })
})

test_that("print", {
  testthat::skip_on_cran()

  SW({
  expect_output(expect_pass_message(fuzz("list", list(NA, c(1, 2, 3)))),
                "OK 2")
  expect_output(expect_fail_message(fuzz("ls", list(NA, c(1, 2, 3)))),
                "FAIL 2")
  })
})
