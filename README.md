
# CBTF: Caught by the Fuzz! <a href="https://www.youtube.com/watch?v=uJ-mpul94eo"><img src="man/figures/logo.png" align="right" height="120" /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

A very simple mechanism to fuzz-test the exported functions of an R
package. At the moment this is extremely limited: it operates only on
the first argument and it doesn’t introduce any randomness. However,
it’s convenient when there are a large number of functions to test.

## Usage

``` r
library(CBTF)
funs <- get_exported_functions("mime")
fuzz(funs, TRUE)
```

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
