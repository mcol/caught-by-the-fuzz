# Print the results from a fuzz run

This formats the results from a fuzz run with colours and prints them to
the terminal. By default, only tests labelled as `"FAIL"` or `"WARN"`
are displayed, but this can be controlled via the `show` argument.

## Usage

``` r
# S3 method for class 'cbtf'
print(x, show = c("fail", "warn"), group = "input", ...)
```

## Arguments

- x:

  An object of class `cbtf`.

- show:

  A character vector representing the subset of results to be printed,
  any of "fail", "warn", "skip", "ok", "all" and "none".

- group:

  Either `"input"` to show results grouped by test input (default), or
  `"function"` to show them grouped by function name.

- ...:

  Further arguments passed to or from other methods. These are currently
  ignored.

## Value

No return value, called for side effects.

## Details

The use of unicode icons in the output messages can be disabled by
setting `options(cli.unicode = FALSE)`.

## See also

[summary.cbtf](https://mcol.github.io/caught-by-the-fuzz/reference/summary.cbtf.md)

## Examples

``` r
res <- fuzz(funs = c("list", "matrix", "mean"),
            what = test_inputs(c("numeric", "raw")))
#> ℹ Fuzzing 3 functions with 10 inputs (using 2 daemons)
#> ℹ Functions will be searched in the global namespace as `package` was not specified
#> ℹ 30 tests run  [19ms]
print(res)
#> ✖  🚨   CAUGHT BY THE FUZZ!   🚨
#> 
#> ── Test input [[7]]: charToRaw("0") 
#>    mean  WARN  argument is not numeric or logical: returning NA
#> 
#> ── Test input [[8]]: charToRaw(NA_character_) 
#>    mean  WARN  argument is not numeric or logical: returning NA
#> 
#> ── Test input [[9]]: charToRaw("abc") 
#>    mean  WARN  argument is not numeric or logical: returning NA
#> 
#> ── Test input [[10]]: raw() 
#>    mean  WARN  argument is not numeric or logical: returning NA
#> 
#>  [ FAIL 0 | WARN 4 | SKIP 0 | OK 26 ] 
print(res, show = "all")
#> ✖  🚨   CAUGHT BY THE FUZZ!   🚨
#> 
#> ── Test input [[1]]: c(1.309605, 0.585381, -0.461072) 
#>    list    OK  
#>  matrix    OK  
#>    mean    OK  
#> 
#> ── Test input [[2]]: c(-1, 0, NaN, 10000) 
#>    list    OK  
#>  matrix    OK  
#>    mean    OK  
#> 
#> ── Test input [[3]]: c(Inf, -0.5, 1234) 
#>    list    OK  
#>  matrix    OK  
#>    mean    OK  
#> 
#> ── Test input [[4]]: c(0, 0) 
#>    list    OK  
#>  matrix    OK  
#>    mean    OK  
#> 
#> ── Test input [[5]]: c(0, NA) 
#>    list    OK  
#>  matrix    OK  
#>    mean    OK  
#> 
#> ── Test input [[6]]: numeric() 
#>    list    OK  
#>  matrix    OK  
#>    mean    OK  
#> 
#> ── Test input [[7]]: charToRaw("0") 
#>    list    OK  
#>  matrix    OK  
#>    mean  WARN  argument is not numeric or logical: returning NA
#> 
#> ── Test input [[8]]: charToRaw(NA_character_) 
#>    list    OK  
#>  matrix    OK  
#>    mean  WARN  argument is not numeric or logical: returning NA
#> 
#> ── Test input [[9]]: charToRaw("abc") 
#>    list    OK  
#>  matrix    OK  
#>    mean  WARN  argument is not numeric or logical: returning NA
#> 
#> ── Test input [[10]]: raw() 
#>    list    OK  
#>  matrix    OK  
#>    mean  WARN  argument is not numeric or logical: returning NA
#> 
#>  [ FAIL 0 | WARN 4 | SKIP 0 | OK 26 ] 
print(res, show = "none")
#> [ FAIL 0 | WARN 4 | SKIP 0 | OK 26 ] 
print(res, group = "function")
#> ✖  🚨   CAUGHT BY THE FUZZ!   🚨
#> 
#> ── Function `mean`: 
#>  WARN  argument is not numeric or logical: returning NA | charToRaw("0")
#>  WARN  argument is not numeric or logical: returning NA | charToRaw(NA_character_)
#>  WARN  argument is not numeric or logical: returning NA | charToRaw("abc")
#>  WARN  argument is not numeric or logical: returning NA | raw()
#> 
#>  [ FAIL 0 | WARN 4 | SKIP 0 | OK 26 ] 
```
