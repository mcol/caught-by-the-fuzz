# Caught by the Fuzz!

A very simple mechanism to fuzz test the exported functions of an R package.

## Usage

```R
funs <- get_exported_functions("Luminescence")
fuzz(funs, TRUE)
```
