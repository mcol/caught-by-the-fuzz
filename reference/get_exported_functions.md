# Get the names of the exported functions of a package

This function extracts the exports from the namespace of the given
package via
[getNamespaceExports](https://rdrr.io/r/base/ns-reflect.html) and
discards non-fuzzable objects (non-functions and functions with no
arguments). The set of names returned can be further restricted via the
`ignore_names` and `ignore_deprecated` arguments.

## Usage

``` r
get_exported_functions(package, ignore_names = "", ignore_deprecated = TRUE)
```

## Arguments

- package:

  Name of the package to fuzz-test.

- ignore_names:

  Names of functions to ignore: these are removed from the names
  returned. This can be helpful, for example, to discard function
  aliases.

- ignore_deprecated:

  Whether deprecated function should be ignored (`TRUE` by default).

## Value

A character vector of the names of the fuzzable functions exported from
the given package, with the `"package"` attribute set. This can be used
directly as the `funs` argument of
[fuzz](https://mcol.github.io/caught-by-the-fuzz/reference/fuzz.md)
without need to specify the `package` argument.

## See also

[fuzz](https://mcol.github.io/caught-by-the-fuzz/reference/fuzz.md)

## Examples

``` r
## get the fuzzable functions in the public interface of this package
funs <- get_exported_functions("CBTF")
```
