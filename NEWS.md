## Current development version


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
