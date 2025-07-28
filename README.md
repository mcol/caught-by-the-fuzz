
# CBTF: Caught by the Fuzz! A minimalistic fuzz-test runner for R<a href="https://www.youtube.com/watch?v=uJ-mpul94eo"><img src="man/figures/logo.png" align="right" height="120" /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The `CBTF` package implements a very simple mechanism for fuzz-testing
functions in the public interface of an R package.

Fuzz testing helps identify functions lacking sufficient argument
validation, and uncovers sets of inputs that, while valid by function
signature, may cause issues within the function body.

The core functionality of the package is `fuzz()`, whose aim is to call
each provided function with a certain input and record the output
produced. If an error is generated, this is captured and reported to the
user, unless the error message matches a pattern of whitelisted errors.

The helper function `get_exported_functions()` identifies the functions
in the public interface of a given package, facilitating the generation
of the list of functions to be fuzzed.

Function `test_inputs()` by default generates a large set of potentially
problematic inputs, but they can be limited just to the desired classes
of inputs.

At the moment this is extremely limited: it operates only on the first
argument and it doesn’t introduce any randomness. However, it’s
convenient when there are a large number of functions to test.

## Usage

``` r
library(CBTF)
funs <- get_exported_functions("mime")
fuzz(funs, what = list(TRUE))
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

## Advanced uses

### Better-looking output

When the inputs contains complex structures, it is better to provide a
named list to the `what` argument of `fuzz()`: these names will be used
instead of relying on deparsing of the input, which may be poor.

For example, compare this:

``` r
fuzz(funs, what = list(letters))
```

    ## ℹ Fuzzing 2 functions on 1 input
    ## ✖  🚨   CAUGHT BY THE FUZZ!   🚨
    ## 
    ## ── Test input: c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l",
    ##  parse_multipart  FAIL  $ operator is invalid for atomic vectors
    ## 
    ##  [ FAIL 1 | WARN 0 | SKIP 0 | OK 1 ]

to this:

``` r
fuzz(funs, what = list(letters = letters))
```

    ## ℹ Fuzzing 2 functions on 1 input
    ## ✖  🚨   CAUGHT BY THE FUZZ!   🚨
    ## 
    ## ── Test input: letters
    ##  parse_multipart  FAIL  $ operator is invalid for atomic vectors
    ## 
    ##  [ FAIL 1 | WARN 0 | SKIP 0 | OK 1 ]

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
website](https://r-lum.github.io/REPLAY-website/).
