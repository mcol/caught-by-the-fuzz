# Current development version

- Return an object with the raw results from `fuzz()` and provide S3 methods
  for `summary()` and `print()` (#1).
- Skip functions that call `readline()`, as in an interactive session they
  stall waiting for user input (#4).
- Skip non-existing function names and functions that accept no arguments.

# CBTF 0.1.0 (2024-11-14)

- First version of the package.
- The package website is now available at
  https://mcol.github.io/caught-by-the-fuzz/
