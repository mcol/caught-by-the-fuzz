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

  ## issue 24
  SW({
  res <- fuzz(c("mean", "median"), list(NA, NULL, 123))
  sum <- summary(res, tabulate = FALSE)[, c("fun", "what")]
  })
  expect_equal(sum$fun,
               c("mean", "median", "mean", "median", "mean", "median"))
  expect_equal(sum$what,
               c("NA", "NA", "NULL", "NULL", "123", "123"))
})

test_that("print", {
  testthat::skip_on_cran()

  SW({
  expect_output(expect_pass_message(fuzz("list", list(NA, c(1, 2, 3)))),
                "OK 2")
  expect_output(expect_fail_message(fuzz("ls", list(NA, c(1, 2, 3)))),
                "FAIL 2")

  ## no failures
  res <- fuzz("list", list(NA, c(1, 2, 3)))
  expect_snapshot(print(res))
  expect_snapshot(print(res, group = "function", show = "all"))

  ## with failures and skips
  res <- fuzz(c("list", "median", "Sys.date"), list(NA), args = list(1:3, TRUE))

  ## empty object
  expect_message(print(subset(res, "no_matching_pattern")),
                 "The object contains no results, probably because")

  expect_error(print(res, show = NA),
               "[print] 'show' should be of class character",
               fixed = TRUE)
  expect_snapshot(print(res))
  expect_snapshot(print(res, show = "all"))
  expect_snapshot(print(res, show = "skip"))
  expect_snapshot(print(res, show = "none"))
  expect_snapshot(print(res, group = "function", show = "all"))
  })
})

test_that("subset", {
  testthat::skip_on_cran()

  SW({
  res <- fuzz(c("list", "median"), list(letters, 1:3))

  ## filter by message pattern
  expect_output(subset(res, msg_patterns = "argument"),
                "argument is not numeric or logical")

  ## filter by function pattern
  expect_output(subset(res, fun_patterns = "median"),
                "argument is not numeric or logical")

  ## combine both filters
  expect_output(subset(res, msg_patterns = "argument", fun_patterns = "median"),
                "argument is not numeric or logical")

  ## group by function
  expect_output(subset(res, msg_patterns = "argument",
                       fun_patterns = "median", group = "function"),
                "argument is not numeric or logical")
  })

  ## no matching messages
  expect_silent(sub <- subset(res, msg_patterns = "xyz_no_match"))
  expect_length(sub, 0)

  expect_error(subset(res, msg_patterns = 123),
               "[subset] 'msg_patterns' should be of class character",
               fixed = TRUE)
  expect_error(subset(res, fun_patterns = 123),
               "[subset] 'fun_patterns' should be of class character",
               fixed = TRUE)
})

test_that("[[", {
  testthat::skip_on_cran()

  SW({
  res <- fuzz("list", list(NA, c(1, 2, 3)))
  expect_s3_class(res[[1]],
                  "data.frame")
  expect_equal(colnames(res[[1]]),
               c("res", "msg", "fun", "what"))
  expect_null(res[[0]])
  expect_null(res[[3]])
  expect_null(res[[10]])
  })
})
