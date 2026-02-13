# Print the results from a fuzz run

This formats the results from a fuzz run with colours and prints them to
the terminal.

## Usage

``` r
# S3 method for class 'cbtf'
print(x, show = c("fail", "warn"), ...)
```

## Arguments

- x:

  An object of class `cbtf`.

- show:

  A character vector representing the subset of results be printed, any
  of "fail", "warn", "skip", "ok" and "all".

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
#> ℹ Fuzzing 3 functions with 9 inputs (using 2 daemons)
#> ℹ Functions will be searched in the global namespace as `package` was not specified
#> ℹ 27 tests run  [23ms]
print(res)
#> ✖  🚨   CAUGHT BY THE FUZZ!   🚨
#> 
#> ── Test input [[7]]: charToRaw("0") 
#>    mean  WARN  argument is not numeric or logical: returning NA
#> 
#> ── Test input [[8]]: charToRaw("abc") 
#>    mean  WARN  argument is not numeric or logical: returning NA
#> 
#> ── Test input [[9]]: raw() 
#>    mean  WARN  argument is not numeric or logical: returning NA
#> 
#>  [ FAIL 0 | WARN 3 | SKIP 0 | OK 24 ] 
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
#> ── Test input [[8]]: charToRaw("abc") 
#>    list    OK  
#>  matrix    OK  
#>    mean  WARN  argument is not numeric or logical: returning NA
#> 
#> ── Test input [[9]]: raw() 
#>    list    OK  
#>  matrix    OK  
#>    mean  WARN  argument is not numeric or logical: returning NA
#> 
#>  [ FAIL 0 | WARN 3 | SKIP 0 | OK 24 ] 
```
