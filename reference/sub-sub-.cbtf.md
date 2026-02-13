# Extract the results for a specific test input

Extract the results for a specific test input

## Usage

``` r
# S3 method for class 'cbtf'
x[[i]]
```

## Arguments

- x:

  An object of class `cbtf`.

- i:

  An index between 1 and the number of test inputs used.

## Value

If the index is valid, a data frame containing the following columns and
attributes:

- res:

  One of "OK", "FAIL", "WARN" or "SKIP" for each combination of function
  and input tested (see the *Value* section in
  [fuzz](https://mcol.github.io/caught-by-the-fuzz/reference/fuzz.md)).

- msg:

  The message received in case of error, warning or skip, or an empty
  string if no failure occurred.

- attr(\*, "what"):

  The character representation of the input tested.

Otherwise, `FALSE`.

## Examples

``` r
res <- fuzz(funs = c("list", "matrix", "mean"),
            what = test_inputs(c("numeric", "raw")))
#> ℹ Fuzzing 3 functions with 9 inputs (using 2 daemons)
#> ℹ Functions will be searched in the global namespace as `package` was not specified
#> ℹ 27 tests run  [24ms]
res[[6]]
#>   res msg
#> 1  OK    
#> 2  OK    
#> 3  OK    
```
