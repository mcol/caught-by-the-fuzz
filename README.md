
# CBTF: Caught by the Fuzz! <a href="https://www.youtube.com/watch?v=uJ-mpul94eo"><img src="man/figures/logo.png" align="right" height="120" /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The `CBTF` package implementa a very simple mechanism for fuzz-testing
functions in the public interface of an R package.

Fuzz testing can be used in the first instance to identify functions
that do not have sufficient argument validation. Besides, fuzz testing
can identify sets of inputs that, while satisfying the implicit typing
of a function signature, are problematic inside the function body. These
often concern presence of missing values, `NULL` entries,
dimensionality- or sign-related errors.

The core functionality of the package is `fuzz()`, whose aim is to call
each provided function with a certain input and record the output
produced. If an error is generated, this is captured and reported to the
user, unless the error message matches a pattern of whitelisted errors.

The helper function `get_exported_functions()` identifies the functions
in the public interface of a given package, facilitating the generation
of the list of functions to be fuzzed.

At the moment this is extremely limited: it operates only on the first
argument and it doesn’t introduce any randomness. However, it’s
convenient when there are a large number of functions to test.

## Usage

``` r
library(CBTF)
funs <- get_exported_functions("mime")
fuzz(funs, list(TRUE))
```

    ## ℹ Fuzzing 3 functions on 1 input
    ## ✖  🚨   CAUGHT BY THE FUZZ!   🚨
    ## 
    ## ── Test input: TRUE
    ##       guess_type  FAIL  a character vector argument expected
    ##  parse_multipart  FAIL  $ operator is invalid for atomic vectors
    ## 
    ##  [ FAIL 2 | WARN 0 | SKIP 1 | OK 0 ]

The first occurrence is a false positive, as the message returned
indicates that the input was checked and the function returned cleanly.
The second case instead reveals that the function didn’t validate its
input: indeed, it expected an environment, and used the `$` operation on
it without checking.

## Funding

Development of `CBTF` is partially supported through the DFG programme
“REPLAY: REProducible Luminescence Data AnalYses” [No
528704761](https://gepris.dfg.de/gepris/projekt/528704761?language=en)
led by Dr Sebastian Kreutzer (PI at Heidelberg University, DE) and Dr
Thomas Kolb (PI at Justus-Liebig-University Giessen, DE). Updates on the
REPLAY project at large are available at the [REPLAY
website](https://replay.r-luminescence.org/).
