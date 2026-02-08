# Current development version

## New features

- Rewrite `fuzz()` to use a backend based on the `mirai` package. This allows
  to parallelize computations and support a timeout to stop non-failing
  functions (#4).
- Add the `timeout` argument to `fuzz()` to control the number of seconds to
  wait before interrupting the execution of a function being fuzzed.
- Add the `args` argument to allow fuzzing multiple function arguments instead
  of only the first, with support for named and fixed arguments (#6, #7, #8).
- Add support for `what = NULL`, which allows running the set of arguments
  listed in `args` with no modifications. This can be used after a fuzz run to
  collect results on just one set of problematic inputs.
- Replace the `show_all` logical argument of `print()` with a `show` argument
  that accepts the class of results to be printed, any of "fail", "warn",
  "skip", "ok" or "all" (#9).

## Updates

- Use unicode symbols only on terminals that support UTF8, and respect the
  `cli.unicode` option, so that `options(cli.unicode = FALSE)` can be used to
  disable all unicode output.
- Fix bug that caused failures in the `+()` operator to be whitelisted just
  because the `+` was interpreted as a regular expression quantifier.
- Ignore deprecated functions by default in `get_exported_functions()`.
- Add more inputs to `test_inputs()`, bringing the total number of inputs from
  60 to 70.
- Move the NA inputs in `test_inputs()` from the "scalar" to the new "na"
  class.
- Add the `[[` operator to extract the results for a specific test input.
- Stop exporting the `namify()` helper.

# CBTF 0.5.0 (2025-08-21)

- Streamline the progress report from `fuzz()`.
- In case both an error and a warning are raised by the function being
  fuzzed, we now report the error; previously the warning was reported
  because of how `tryCatch()` is implemented.
- The `length()` S3 method for objects of class `cbtf` now counts the total
  number of tests performed, instead of the number of inputs tested.
- Add the `tabulate` argument to `summary()` to control whether the tabulation
  of results should be printed to the terminal or not.
- Change the internal structure of the `cbtf` object to store the names of
  the functions fuzzed only once.
- Store the values of the `ignore_patterns` and `ignore_warnings` options in
  the `cbtf` object.
- Add the `whitelist()` function to remove false positives from a `cbtf`
  object.
- Make `test_input()` recognise the `"help"` keyword, in which case it returns
  a vector of valid input classes.
- Add the `namify()` function to help create a named list of custom input
  objects.

# CBTF 0.4.0 (2025-07-31)

- Don't crash in `get_exported_functions()` if `getNameSpaceExports()` fails.
- Store the error message in the results object also in case of whitelisted
  errors, so it can be reported from `print(..., show_all = TRUE)`.
- Add the tabulation shown in `summary()` as attribute `"summary_table"` of
  the data frame returned.
- Make `get_exported_functions()` return only fuzzable functions, so that
  functions with no arguments are no longer returned.
- Expand the documentation to satisfy the CRAN request.

# CBTF 0.3.0 (2025-07-27)

- Warnings containing the name of the function being fuzzed are no longer
  reported, to make their behaviour consistent with that of errors.
- Add the `package` argument to `fuzz()` as an alternative and simpler way
  to specify the namespace where function names are searched.
- Don't consider primitives (such as `list()` or `+`) to be functions with no
  arguments (#5).
- Make the `what` argument to `fuzz()` accept a list of inputs, so that
  the function can test multiple inputs in the same run.
- Add function `test_inputs()` to return a configurable list of problematic
  inputs.
- Add the `listify_what` argument to `fuzz()` so that each input in `what`
  is also tested in its listified version.
- Provide the S3 method for `length()`.
- Make the output from `summary()` more informative and include a column with
  the inputs tested in the data frame returned.

# CBTF 0.2.0 (2025-07-16)

- Return an object with the raw results from `fuzz()` and provide S3 methods
  for `summary()` and `print()` (#1).
- Skip functions that call `readline()`, as in an interactive session they
  stall waiting for user input (#4).
- Skip non-existing function names and functions that accept no arguments.
- Show a progress bar during `fuzz()`.
- Prettify the output from `fuzz()`, `summary()` and `print()`.

# CBTF 0.1.0 (2024-11-14)

- First version of the package.
- The package website is now available at
  https://mcol.github.io/caught-by-the-fuzz/
