# Apply additional whitelist patterns to the results of a fuzz run

This allows for post-hoc whitelisting of results according to the
patterns specified.

## Usage

``` r
whitelist(object, patterns)
```

## Arguments

- object:

  An object of class `cbtf`.

- patterns:

  One or more strings containing regular expressions to match the errors
  to whitelist.

## Value

An object of class `cbtf` with the additional whitelist patterns
applied.

## See also

[fuzz](https://mcol.github.io/caught-by-the-fuzz/reference/fuzz.md)

## Examples

``` r
## this reports a false positive result
(res <- fuzz(funs = "matrix", what = test_inputs("scalar")))
#> ℹ Fuzzing 1 function with 10 inputs (using 2 daemons)
#> ℹ Functions will be searched in the global namespace as `package` was not specified
#> ℹ 10 tests run  [13ms]
#> ✖  🚨   CAUGHT BY THE FUZZ!   🚨
#> 
#> ── Test input [[10]]: NULL 
#>  matrix  FAIL  'data' must be of a vector type, was 'NULL'
#> 
#>  [ FAIL 1 | WARN 0 | SKIP 0 | OK 9 ] 

## with whitelisting, we can remove that
whitelist(res, "must be of a vector type")
#> ✔  🏃 You didn't get caught by the fuzz!
#> 
#>  [ FAIL 0 | WARN 0 | SKIP 0 | OK 10 ] 
```
