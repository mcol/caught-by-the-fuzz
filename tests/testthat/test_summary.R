test_that("summary", {
  testthat::skip_on_cran()

  SW({
  res <- fuzz("list", list(NA, c(1, 2, 3)))
  sum <- summary(res)
  expect_s3_class(sum,
                  "data.frame")
  expect_named(sum,
               c("fun", "what", "res", "msg"))
  expect_equal(sum$what,
               c("NA", "c(1, 2, 3)"))
  })
  expect_s3_class(attr(sum, "summary_table"),
                  "table")

  expect_error(summary(res, tabulate = NA),
               "[summary] 'tabulate' should be of class logical",
               fixed = TRUE)
  expect_error(summary(res, tabulate = c(TRUE, FALSE)),
               "[summary] 'tabulate' should be a single logical value",
               fixed = TRUE)
  expect_snapshot(summary(res))
  expect_snapshot(summary(res, tabulate = FALSE))
})

test_that("print", {
  testthat::skip_on_cran()

  SW({
  expect_output(expect_pass_message(fuzz("list", list(NA, c(1, 2, 3)))),
                "OK 2")
  expect_output(expect_fail_message(fuzz("ls", list(NA, c(1, 2, 3)))),
                "FAIL 2")

  res <- fuzz(c("list", "median"), list(NA))

  expect_error(print(res, show = NA),
               "[print] 'show' should be of class character",
               fixed = TRUE)
  expect_snapshot(print(res))
  expect_snapshot(print(res, show = "all"))
  expect_snapshot(print(res, show = "skip"))
  })
})

test_that("[[", {
  testthat::skip_on_cran()

  SW({
  res <- fuzz("list", list(NA, c(1, 2, 3)))
  expect_s3_class(res[[1]],
                  "data.frame")
  expect_equal(colnames(res[[1]]),
               c("res", "msg"))
  expect_equal(attr(res[[1]], "what"),
               "NA")
  expect_false(res[[0]])
  expect_false(res[[10]])
  })
})
