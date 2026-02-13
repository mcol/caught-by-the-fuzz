# Compute the number of tests performed

Compute the number of tests performed

## Usage

``` r
# S3 method for class 'cbtf'
length(x)
```

## Arguments

- x:

  An object of class `cbtf`.

## Value

An integer corresponding to the number of tests performed in a run.

## Examples

``` r
res <- fuzz(funs = c("list", "matrix", "mean"),
            what = test_inputs(c("numeric", "raw")))
#> ℹ Fuzzing 3 functions with 9 inputs (using 2 daemons)
#> ℹ Functions will be searched in the global namespace as `package` was not specified
#> ℹ 27 tests run  [23ms]
length(res)
#> [1] 27
```
