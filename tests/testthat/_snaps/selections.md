# simple name selections

    Code
      recipes_eval_select(quos = quos(log(date)), data = okc, info = info1)
    Condition
      Error in `log()`:
      ! non-numeric argument to mathematical function

---

    Code
      recipes_eval_select(data = okc, info = info1)
    Condition
      Error in `enexpr()`:
      ! argument "quos" is missing, with no default

