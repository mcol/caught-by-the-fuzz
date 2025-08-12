# Current development version

- Streamline the progress report from `fuzz()`.
- In case both an error and a warning are raised by the function being
  fuzzed, we now report the error; previously the warning was reported
  because of how `tryCatch()` is implemented.

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
- Make the output from summary() more informative and include a column with
  the inputs tested in the data frame returned.

# CBTF 0.2.0 (2025-07-16)

- Return an object with the raw results from `fuzz()` and provide S3 methods
  for `summary()` and `print()` (#1).
- Skip functions that call `readline()`, as in an interactive session they
  stall waiting for user input (#4).
- Skip non-existing function names and functions that accept no arguments.
- Show a progress bar during fuzz().
- Prettify the output from fuzz(), summary() and print().

# CBTF 0.1.0 (2024-11-14)

- First version of the package.
- The package website is now available at
  https://mcol.github.io/caught-by-the-fuzz/
