# simple name selections

    Code
      recipes_eval_select(quos = quos(log(date)), data = okc, info = info1)
    Error <rlang_error>
      non-numeric argument to mathematical function
      Caused by error in `log()`:
      ! non-numeric argument to mathematical function

---

    Code
      recipes_eval_select(data = okc, info = info1)
    Error <simpleError>
      argument "quos" is missing, with no default

