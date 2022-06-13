# simple name selections

    Code
      recipes_eval_select(quos = quos(log(beds)), data = Sacramento, info = info1)
    Condition
      Error:
      ! object 'beds' not found

---

    Code
      recipes_eval_select(data = Sacramento, info = info1)
    Condition
      Error in `enexpr()`:
      ! argument "quos" is missing, with no default

