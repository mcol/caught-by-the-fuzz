
# CBTF: Caught by the Fuzz! A minimalistic fuzz-test runner for R<a href="https://www.youtube.com/watch?v=uJ-mpul94eo"><img src="man/figures/logo.png" align="right" height="120" /></a>

<!-- badges: start -->

[![CRAN](https://www.r-pkg.org/badges/version/CBTF)](https://CRAN.R-project.org/package=CBTF)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The `CBTF` package implements a very simple mechanism for fuzz-testing
functions in the public interface of an R package.

Fuzz testing helps identify functions that lack sufficient argument
validation, and uncovers sets of inputs that, while valid by function
signature, may cause issues within the function body.

The core functionality of the package is in the `fuzz()` function, which
calls each provided function with a certain input and records the output
produced. If an error or a warning is generated, this is captured and
reported to the user, unless it matches a pattern of whitelisted
messages, as specified in the `ignore_patterns` argument. The objects
returned by `fuzz()` can be inspected with `summary()` and `print()`.

Whitelisting can also be done after a fuzz run has been completed via
the `whitelist()` function, so that only messages that need to be acted
upon are actually shown. Using `whitelist()` has the advantage of not
requiring to run the fuzzer over all functions and all inputs again.

Note that `fuzz()` uses the [`mirai` package](https://mirai.r-lib.org/)
for asynchronous operations and parallelisation. Therefore, the function
may be invoked only after persistent background processes are set up.
This can be done with the `daemons()` function, which allows to control
the number of processes to use; refer to the original `mirai`
documentation for a complete description of its arguments and behaviour.

The helper function `get_exported_functions()` identifies the functions
in the public interface of a given package, facilitating the generation
of the list of functions to be fuzzed.

The helper function `test_inputs()` is invoked by `fuzz()` if the user
doesn’t specify the set of inputs to be tested. By default it generates
a large set of potentially problematic inputs, but these can be limited
just to the desired classes of inputs.

The helper function `namify()` can be used to generate automatically
pretty names in the list of input object, which can improve the output,
especially when structures such as data frames, matrices, and more
complex objects are involved. These names are based on the deparsed
representation of the unevaluated inputs.

At the moment the functionality of the package is extremely limited: it
operates only on the first argument and it doesn’t introduce any
randomness. However, it’s convenient when there are a large number of
functions to test.

## Usage

This is a simple example that displays how to use `CBTF` to fuzz an R
package. We consider `mime` because it is small enough to run quickly
and is likely installed on most systems.

``` r
library(CBTF)
funs <- get_exported_functions("mime")
daemons(2)
(res <- fuzz(funs, what = list(TRUE)))
```

    ## ℹ Fuzzing 2 functions on 1 input
    ## ✖  🚨   CAUGHT BY THE FUZZ!   🚨
    ## 
    ## ── Test input: TRUE
    ##       guess_type  FAIL  a character vector argument expected
    ##  parse_multipart  FAIL  $ operator is invalid for atomic vectors
    ## 
    ##  [ FAIL 2 | WARN 0 | SKIP 0 | OK 0 ]

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

    ## ✖  🚨   CAUGHT BY THE FUZZ!   🚨
    ## 
    ## ── Test input: TRUE
    ##  parse_multipart  FAIL  $ operator is invalid for atomic vectors
    ## 
    ##  [ FAIL 1 | WARN 0 | SKIP 0 | OK 1 ]

## Advanced uses

### Better-looking output

When the inputs contains complex structures, it is better to provide a
named list to the `what` argument of `fuzz()`: these names will be used
instead of relying on deparsing of the input, which may be poor. A
convenient way of generating names is by using the `namify()` helper
function.

For example, compare this:

``` r
fuzz(funs, what = list(letters, data.frame(a = 1, b = "a")))
```

    ## ℹ Fuzzing 2 functions on 2 inputs
    ## ✖  🚨   CAUGHT BY THE FUZZ!   🚨
    ## 
    ## ── Test input: c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l",
    ##  parse_multipart  FAIL  $ operator is invalid for atomic vectors
    ## 
    ## ── Test input: structure(list(a = 1, b = "a"), class = "data.frame", row.names = c(NA,
    ##       guess_type  FAIL  a character vector argument expected
    ## 
    ##  [ FAIL 2 | WARN 0 | SKIP 0 | OK 2 ]

to this:

``` r
fuzz(funs, what = namify(letters, data.frame(a = 1, b = "a")))
```

    ## ℹ Fuzzing 2 functions on 2 inputs
    ## ✖  🚨   CAUGHT BY THE FUZZ!   🚨
    ## 
    ## ── Test input: letters
    ##  parse_multipart  FAIL  $ operator is invalid for atomic vectors
    ## 
    ## ── Test input: data.frame(a = 1, b = "a")
    ##       guess_type  FAIL  a character vector argument expected
    ## 
    ##  [ FAIL 2 | WARN 0 | SKIP 0 | OK 2 ]

### Controlling the inputs tested

By default, `fuzz()` tests all the inputs produced by `test_inputs()`.
However, this can be controlled by specifying the classes that should be
tested:

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

    ##  [1] "all"        "scalar"     "numeric"    "integer"    "logical"   
    ##  [6] "character"  "factor"     "data.frame" "matrix"     "array"     
    ## [11] "date"       "raw"        "list"

### Fuzzing list inputs automatically

It is trivial to augment a given set of inputs with list versions of the
same. This effectively doubles the number of tests run with no
additional coding effort.

``` r
fuzz(funs, what = namify(letters), listify_what = TRUE)
```

    ## ℹ Fuzzing 2 functions on 2 inputs
    ## ✖  🚨   CAUGHT BY THE FUZZ!   🚨
    ## 
    ## ── Test input: letters
    ##  parse_multipart  FAIL  $ operator is invalid for atomic vectors
    ## 
    ## ── Test input: list(letters)
    ##       guess_type  FAIL  a character vector argument expected
    ## 
    ##  [ FAIL 2 | WARN 0 | SKIP 0 | OK 2 ]

### Fuzzing for arguments other than the first

At the moment, the only way to fuzz an argument other than the first is
by currying the function, ensuring that the preceding arguments before
are filled in.

For example, to fuzz the `nrow` argument of `matrix()`, we could do the
following:

``` r
curried.matrix <- function(nrow) matrix(1:10, nrow = nrow)
fuzz("curried.matrix", what = list(NA, NULL))
```

    ## ℹ Fuzzing 1 function on 2 inputs
    ## ℹ Functions will be searched in the global namespace as 'package' was not specified
    ## ✖  🚨   CAUGHT BY THE FUZZ!   🚨
    ## 
    ## ── Test input: NA
    ##  curried.matrix  FAIL  invalid 'nrow' value (too large or NA)
    ## 
    ## ── Test input: NULL
    ##  curried.matrix  FAIL  non-numeric matrix extent
    ## 
    ##  [ FAIL 2 | WARN 0 | SKIP 0 | OK 0 ]

## Funding

Development of `CBTF` is partially supported through the DFG programme
“REPLAY: REProducible Luminescence Data AnalYses” [No
528704761](https://gepris.dfg.de/gepris/projekt/528704761?language=en)
led by Dr Sebastian Kreutzer (PI at Heidelberg University, DE) and Dr
Thomas Kolb (PI at Justus-Liebig-University Giessen, DE). Updates on the
REPLAY project at large are available at the [REPLAY
website](https://replay.geog.uni-heidelberg.de/).
