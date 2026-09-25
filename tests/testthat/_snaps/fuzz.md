# check object returned

    Code
      fuzz()
    Condition
      Error in `fuzz()`:
      ! `funs` should be of class <character>

---

    Code
      fuzz("mean", what = NULL, args = NULL)
    Condition
      Error in `fuzz()`:
      ! `what` and `args` cannot be both NULL

---

    Code
      fuzz("mean", timeout = 0)
    Condition
      Error in `fuzz()`:
      ! `timeout` should be at least 0.5

# whitelist

    Code
      whitelist(res, NA)
    Condition
      Error in `whitelist()`:
      ! `patterns` should be of class <character>

---

    Code
      whitelist(res, character(0))
    Condition
      Error in `whitelist()`:
      ! `patterns` is an empty <character>

# get_exported_functions

    Code
      get_exported_functions("nonexistent")
    Condition
      Error in `get_exported_functions()`:
      ! there is no package called 'nonexistent'

---

    Code
      get_exported_functions("CBTF", character(0))
    Condition
      Error in `get_exported_functions()`:
      ! `ignore_names` is an empty <character>

