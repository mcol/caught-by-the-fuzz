# Current development version

- Warnings containing the name of the function being fuzzed are no longer
  reported, to make their behaviour consistent with that of errors.
- Add the `package` argument to `fuzz()` as an alternative and simpler way
  to specify the namespace where function names are searched.
- Don't consider primitives (such as `list()` or `+`) to be functions with no
  arguments (#5).
- Make the `what` argument to `fuzz()` accept a list of inputs, so that
  the function can test multiple inputs in the same run.
- Add a default list of inputs to `fuzz()`.

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
