# Caught by the Fuzz <a href="https://www.youtube.com/watch?v=uJ-mpul94eo"><img src="man/figures/logo.png" align="right" height="120" /></a>

A very simple mechanism to fuzz test the exported functions of an R package.

## Usage

```R
funs <- get_exported_functions("Luminescence")
fuzz(funs, TRUE)
```
