
# CBTF: Caught by the Fuzz\! <a href="https://www.youtube.com/watch?v=uJ-mpul94eo"><img src="man/figures/logo.png" align="right" height="120" /></a>

A very simple mechanism to fuzz-test the exported functions of an R
package. At the moment this is extremely limited: it operates only on
the first argument and it doesn’t introduce any randomness. However,
it’s convenient when there are a large number of functions to test.

## Usage

``` r
library(CBTF)
funs <- get_exported_functions("glue")
what <- TRUE
fuzz(funs, what)
```

    ## 
    ##  🚨   CAUGHT BY THE FUZZ!   🚨
    ## 
    ## FAIL: glue_data ( logical )
    ##     names(x) must be a character vector of the same length as x 
    ## 
    ## FAIL: glue_data_col ( logical )
    ##     names(x) must be a character vector of the same length as x 
    ## 
    ## FAIL: glue_data_safe ( logical )
    ##     names(x) must be a character vector of the same length as x 
    ## 
    ## FAIL: glue_data_sql ( logical )
    ##     there is no package called 'DBI' 
    ## 
    ## FAIL: glue_sql ( logical )
    ##     there is no package called 'DBI' 
    ## 
    ## FAIL: glue_sql_collapse ( logical )
    ##     there is no package called 'DBI'
