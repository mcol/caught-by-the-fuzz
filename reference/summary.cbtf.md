# Results summary from a fuzz run

Reports some summary statistics from the results of a run of
[fuzz](https://mcol.github.io/caught-by-the-fuzz/reference/fuzz.md).

## Usage

``` r
# S3 method for class 'cbtf'
summary(object, tabulate = TRUE, ...)
```

## Arguments

- object:

  An object of class `cbtf`.

- tabulate:

  Whether a tabulation of results should be printed out (`TRUE` by
  default). The tabulation can always be retrieved from the
  `"summary_table"` attribute of the returned object also when
  `tabulate = FALSE`.

- ...:

  Further arguments passed to or from other methods. These are currently
  ignored.

## Value

A data frame containing the following columns and attributes is returned
invisibly:

- fun:

  The names of the function tested.

- what:

  The inputs tested.

- res:

  One of "OK", "FAIL", "WARN" or "SKIP" for each combination of function
  and input tested (see the *Value* section in
  [fuzz](https://mcol.github.io/caught-by-the-fuzz/reference/fuzz.md)).

- msg:

  The message received in case of error, warning or skip, or an empty
  string if no failure occurred.

- attr(\*, "summary_table"):

  The tabulation of results that was printed out.

## Details

The use of unicode icons in the output messages can be disabled by
setting `options(cli.unicode = FALSE)`.

## See also

[print.cbtf](https://mcol.github.io/caught-by-the-fuzz/reference/print.cbtf.md)

## Examples

``` r
res <- fuzz(funs = c("list", "matrix", "mean"),
            what = test_inputs(c("numeric", "raw")))
#> ℹ Fuzzing 3 functions with 9 inputs (using 2 daemons)
#> ℹ Functions will be searched in the global namespace as `package` was not specified
#> ℹ 27 tests run  [23ms]
summary(res)
#> Fuzzed 3 functions on 9 inputs:
#>         
#>          FAIL WARN SKIP OK
#>   list      0    0    0  9
#>   matrix    0    0    0  9
#>   mean      0    3    0  6
#> 
#> [ FAIL 0 | WARN 3 | SKIP 0 | OK 24 ]
```
