# CBTF: Caught by the Fuzz! A minimalistic fuzz-test runner

This package implements a very simple mechanism for fuzz-testing
functions in the public interface of an R package.

## Details

Fuzz testing helps identify functions that lack sufficient argument
validation, and uncovers sets of inputs that, while valid by function
signature, may cause issues within the function body.

The core functionality of the package is in
[fuzz](https://mcol.github.io/caught-by-the-fuzz/reference/fuzz.md),
which calls each provided function with a certain input and records the
output produced. If an error or a warning is generated, this is captured
and reported to the user, unless it matches a pattern of whitelisted
messages. The objects returned by
[fuzz](https://mcol.github.io/caught-by-the-fuzz/reference/fuzz.md) can
be inspected with
[summary.cbtf](https://mcol.github.io/caught-by-the-fuzz/reference/summary.cbtf.md)
and
[print.cbtf](https://mcol.github.io/caught-by-the-fuzz/reference/print.cbtf.md).

Whitelisting can also be done after a fuzz run has been completed via
the
[whitelist](https://mcol.github.io/caught-by-the-fuzz/reference/whitelist.md)
function, so that only messages that need to be acted upon are actually
shown. Using
[whitelist](https://mcol.github.io/caught-by-the-fuzz/reference/whitelist.md)
has the advantage of not requiring the completion of a fuzz run of all
functions over all inputs again.

Note that
[fuzz](https://mcol.github.io/caught-by-the-fuzz/reference/fuzz.md)
relies on the `mirai` package for asynchronous operations and
parallelisation, and execution occurs on persistent background
processes. These can be started automatically by specifying the
`daemons` option; alternatively, they can be set up manually with the
[`mirai::daemons()`](https://mirai.r-lib.org/reference/daemons.html)
function; refer to the original `mirai` documentation for a complete
description of its arguments and behaviour.

The helper function
[get_exported_functions](https://mcol.github.io/caught-by-the-fuzz/reference/get_exported_functions.md)
identifies the functions in the public interface of a given package,
facilitating the generation of the list of functions to be fuzzed.

The helper function
[test_inputs](https://mcol.github.io/caught-by-the-fuzz/reference/test_inputs.md)
is invoked by
[fuzz](https://mcol.github.io/caught-by-the-fuzz/reference/fuzz.md) if
the user doesn't specify the set of inputs to be tested. By default
generates a large set of potentially problematic inputs, but these can
be limited just to the desired classes of inputs.

## See also

Useful links:

- <https://mcol.github.io/caught-by-the-fuzz/>

- Report bugs at <https://github.com/mcol/caught-by-the-fuzz/issues>

## Author

**Maintainer**: Marco Colombo <mar.colombo13@gmail.com>
([ORCID](https://orcid.org/0000-0001-6672-0623))
