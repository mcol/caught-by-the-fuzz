# CBTF: Caught by the Fuzz! A minimalistic fuzz-test runner for R

The `CBTF` package implements a very simple mechanism for fuzz-testing
functions in the public interface of an R package.

Fuzz testing helps identify functions that lack sufficient argument
validation, and uncovers sets of inputs that, while valid by function
signature, may cause issues within the function body.

The core functionality of the package is in the
[`fuzz()`](https://mcol.github.io/caught-by-the-fuzz/reference/fuzz.md)
function, which calls each provided function with a certain input and
records the output produced. If an error or a warning is generated, this
is captured and reported to the user, unless it matches a pattern of
whitelisted messages, as specified in the `ignore_patterns` argument.
The objects returned by
[`fuzz()`](https://mcol.github.io/caught-by-the-fuzz/reference/fuzz.md)
can be inspected with [`summary()`](https://rdrr.io/r/base/summary.html)
and [`print()`](https://rdrr.io/r/base/print.html).

Whitelisting can also be done after a fuzz run has been completed via
the
[`whitelist()`](https://mcol.github.io/caught-by-the-fuzz/reference/whitelist.md)
function, so that only messages that need to be acted upon are actually
shown. Using
[`whitelist()`](https://mcol.github.io/caught-by-the-fuzz/reference/whitelist.md)
has the advantage of not requiring to run the fuzzer over all functions
and all inputs again.

Note that
[`fuzz()`](https://mcol.github.io/caught-by-the-fuzz/reference/fuzz.md)
uses the [`mirai` package](https://mirai.r-lib.org/) for asynchronous
operations and parallelisation, and execution occurs on persistent
background processes. These can be started automatically by specifying
the `daemons` option; alternatively, they can be set up manually with
the [`mirai::daemons()`](https://mirai.r-lib.org/reference/daemons.html)
function; refer to the original `mirai` documentation for a complete
description of its arguments and behaviour.

The helper function
[`get_exported_functions()`](https://mcol.github.io/caught-by-the-fuzz/reference/get_exported_functions.md)
identifies the functions in the public interface of a given package,
facilitating the generation of the list of functions to be fuzzed.

The helper function
[`test_inputs()`](https://mcol.github.io/caught-by-the-fuzz/reference/test_inputs.md)
is invoked by
[`fuzz()`](https://mcol.github.io/caught-by-the-fuzz/reference/fuzz.md)
if the user doesn’t specify the set of inputs to be tested. By default
it generates a large set of potentially problematic inputs, but these
can be limited just to the desired classes of inputs.

## Usage

This is a simple example that displays how to use `CBTF` to fuzz an R
package. We consider `mime` because it is small enough to run quickly
and is likely installed on most systems.

``` r
library(CBTF)
funs <- get_exported_functions("mime")
(res <- fuzz(funs, what = list(TRUE)))
```

``` R
## ℹ Fuzzing 2 functions with 1 input (using 2 daemons)
## ℹ 2 tests run  [4ms]
## ✖  🚨   CAUGHT BY THE FUZZ!   🚨
## 
## ── Test input [[1]]: TRUE
##       guess_type  FAIL  a character vector argument expected
##  parse_multipart  FAIL  $ operator is invalid for atomic vectors
## 
##  [ FAIL 2 | WARN 0 | SKIP 0 | OK 0 ]
```

The first occurrence is a false positive, as the message returned
indicates that the input was checked and the function returned cleanly.
The second case instead reveals that the function didn’t validate its
input: indeed, it expected an environment, and used the `$` operation on
it without checking.

The false positive result can be easily removed by whitelisting an
appropriate pattern:

``` r
whitelist(res, "a character vector argument expected")
```

``` R
## ✖  🚨   CAUGHT BY THE FUZZ!   🚨
## 
## ── Test input [[1]]: TRUE
##  parse_multipart  FAIL  $ operator is invalid for atomic vectors
## 
##  [ FAIL 1 | WARN 0 | SKIP 0 | OK 1 ]
```

### Fuzzing multiple arguments

The example above didn’t specify the `args` argument, and therefore only
the first function argument was fuzzed. By setting `args`, it’s possible
to provide a list of arguments (of any length) to pass to the functions
being fuzzed.

To fuzz the first three arguments of
[`matrix()`](https://rdrr.io/r/base/matrix.html) (`data`, `nrow` and
`ncol`), we could do the following:

``` r
fuzz("matrix", what = list(NA, NULL), args = list(1:4, 2, 2))
```

``` R
## ℹ Fuzzing 1 function with 6 inputs (using 2 daemons)
## ℹ Functions will be searched in the global namespace as `package` was not specified
## ℹ 6 tests run  [6ms]
## ✖  🚨   CAUGHT BY THE FUZZ!   🚨
## 
## ── Test input [[2]]: NULL, 2, 2
##  matrix  FAIL  'data' must be of a vector type, was 'NULL'
## 
## ── Test input [[3]]: 1:4, NA, 2
##  matrix  FAIL  invalid 'nrow' value (too large or NA)
## 
## ── Test input [[5]]: 1:4, 2, NA
##  matrix  FAIL  invalid 'ncol' value (too large or NA)
## 
##  [ FAIL 3 | WARN 0 | SKIP 0 | OK 3 ]
```

If names are given to elements in `args`, they will be used in the input
lists generated. This can be helpful to fuzz a specific argument across
several function, or to fuzz an argument that comes later in the
argument list or that are passed via `...`.

For example, the following will fuzz the `data`, `nrow` and `dimnames`
arguments:

``` r
fuzz("matrix", what = list(NA, NULL), args = list(1:4, 2, dimnames = NULL))
```

``` R
## ℹ Fuzzing 1 function with 6 inputs (using 2 daemons)
## ℹ Functions will be searched in the global namespace as `package` was not specified
## ℹ 6 tests run  [6ms]
## ✖  🚨   CAUGHT BY THE FUZZ!   🚨
## 
## ── Test input [[2]]: NULL, 2, dimnames = NULL
##  matrix  FAIL  'data' must be of a vector type, was 'NULL'
## 
## ── Test input [[3]]: 1:4, NA, dimnames = NULL
##  matrix  FAIL  invalid 'nrow' value (too large or NA)
## 
## ── Test input [[5]]: 1:4, 2, dimnames = NA
##  matrix  FAIL  'dimnames' must be a list
## 
##  [ FAIL 3 | WARN 0 | SKIP 0 | OK 3 ]
```

Fuzzing is generally more effective when the values in `args` provide a
good default for the functions being fuzzed; while an argument is being
fuzzed, the others remain valid, allowing for a broader exploration of
the argument space.

However, this is not strictly necessary (and may not be achievable when
fuzzing multiple functions simultaneously). Additionally, some code
paths may only be reached through combinations of invalid values that
would otherwise remain unexplored.

## Advanced topics

### Generated argument lists

Either `args` or `what` can be `NULL`, but not both. The most basic use
sets `args = NULL`, in which case each of the inputs in `what` is
assigned to the first argument of each function being fuzzed. For
example, if `what = list(NA, "")`, then the following input lists will
be generated:

- `list(NA)`
- `list("")`

If instead `what = NULL`, the exact argument list defined in `args` will
be tested on all functions without any fuzzing. This can be useful after
a fuzz run to collect results on a specific set of problematic inputs,
or to monitor progress while addressing the bugs discovered during the
fuzzing process.

However, the full potential of `CBTF` is realised by specifying both
`what` (or leaving it unset, which deploys the full range of inputs
generated by
[`test_inputs()`](https://mcol.github.io/caught-by-the-fuzz/reference/test_inputs.md))
and `args`. In such cases, `args` is expanded internally into multiple
lists, each differing by one elements modified according to an element
in `what`.

For example, if `what = list(NA, "")` and
`args = list(x = 11, y = 22, 33)`, the following input lists will be
generated and tested:

- `list(x = NA, y = 22, 33)`
- `list(x = "", y = 22, 33)`
- `list(x = 11, y = NA, 33)`
- `list(x = 11, y = "", 33)`
- `list(x = 11, y = 22, NA)`
- `list(x = 11, y = 22, "")`

Sometimes it may be helpful to specify an argument ho have a fixed
value, so that it remains unchanged while fuzzing. This can be done by
prepending “..” to the argument name. For example, to fix argument `x`
at 11, we would use `args = list(..x = 11, y = 22, 33)`, and only these
inputs will be tested:

- `list(x = 11, y = NA, 33)`
- `list(x = 11, y = "", 33)`
- `list(x = 11, y = 22, NA)`
- `list(x = 11, y = 22, "")`

### Parallel execution

The implementation of
[`fuzz()`](https://mcol.github.io/caught-by-the-fuzz/reference/fuzz.md)
uses the [`mirai` package](https://mirai.r-lib.org/) for asynchronous
operations and parallelisation. This allows to fuzz the functions on
persistent background processes (daemons) in parallel.

There are two main approaches to control parallel execution:

1.  Setting the `daemons` argument to an integer value (2 by default):
    this will start as many daemons as specified (and shut them down
    automatically at the end of the fuzz run). Note that there is no
    benefit in starting more daemons than the number of available cores.

    ``` r
    ## this will start 4 daemons
    res <- fuzz(funs, daemons = 4)
    ```

2.  Manually setting up the daemons before the start of the function:
    this can be accomplished via
    [`mirai::daemons()`](https://mirai.r-lib.org/reference/daemons.html),
    which allows to specify remote daemons as well as local one. This
    also avoids the cost of starting and closing daemons if
    [`fuzz()`](https://mcol.github.io/caught-by-the-fuzz/reference/fuzz.md)
    were to be called multiple times. It remains responsibility of the
    user to close the daemons when no longer in use. When active daemons
    are found, the `daemons` argument will be ignored. Refer to the
    original `mirai` documentation for a complete description of its
    arguments and behaviour.

    ``` r
    ## set up persistent background processes on the local machine
    mirai::daemons(4)
    res <- fuzz(funs)

    ## close the background processes
    mirai::daemons(0)
    ```

### Timeouts

Long-running functions can slow down the progress of
[`fuzz()`](https://mcol.github.io/caught-by-the-fuzz/reference/fuzz.md),
so by default if a function does not produce an error within 2 seconds,
it will be stopped. A timed out function returns an `"OK"` result, with
the corresponding `$msg` field recording that a timeout was applied.

However, the default timeout may be too short (or perhaps too long) in
some applications. If desired, the maximum running time of a job (in
seconds) can be controlled via the `timeout` argument of
[`fuzz()`](https://mcol.github.io/caught-by-the-fuzz/reference/fuzz.md)

### Controlling the inputs tested

By default,
[`fuzz()`](https://mcol.github.io/caught-by-the-fuzz/reference/fuzz.md)
tests all the inputs produced by
[`test_inputs()`](https://mcol.github.io/caught-by-the-fuzz/reference/test_inputs.md)
(currently 70 inputs). However, this can be controlled by specifying the
classes that should be tested:

``` r
test_inputs(use = c("scalar", "numeric", "integer", "matrix"))
```

Alternatively, one can specify the classes to be excluded:

``` r
test_inputs(skip = c("date", "raw"))
```

A vector of valid classes can be retrieved programmatically by setting
this argument to “help”:

``` r
test_inputs("help")
```

``` R
##  [1] "all"        "scalar"     "numeric"    "integer"    "logical"   
##  [6] "character"  "factor"     "data.frame" "matrix"     "array"     
## [11] "date"       "raw"        "na"         "list"
```

### Fuzzing list inputs automatically

It is trivial to augment a given set of inputs with list versions of the
same. This effectively doubles the number of tests run with no
additional coding effort.

``` r
fuzz(funs, what = list(letters), listify_what = TRUE)
```

``` R
## ℹ Fuzzing 2 functions with 2 inputs (using 2 daemons)
## ℹ 4 tests run  [5ms]
## ✖  🚨   CAUGHT BY THE FUZZ!   🚨
## 
## ── Test input [[1]]: letters
##  parse_multipart  FAIL  $ operator is invalid for atomic vectors
## 
## ── Test input [[2]]: list(letters)
##       guess_type  FAIL  a character vector argument expected
## 
##  [ FAIL 2 | WARN 0 | SKIP 0 | OK 2 ]
```

## Funding

Development of `CBTF` is partially supported through the DFG programme
“REPLAY: REProducible Luminescence Data AnalYses” [No
528704761](https://gepris.dfg.de/gepris/projekt/528704761?language=en)
led by Dr Sebastian Kreutzer (PI at LIAG - Institute for Applied
Geophysics, Hannover, DE) and Dr Thomas Kolb (PI at
Justus-Liebig-University Giessen, DE). Updates on the REPLAY project at
large are available at the [REPLAY
website](https://replay.geog.uni-heidelberg.de/).
