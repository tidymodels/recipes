# simple name selections

    Code
      recipes_eval_select(quos = quos(log(beds)), data = Sacramento, info = info_sac)
    Condition
      Error:
      i In argument: `log(beds)`.
      Caused by error:
      ! object 'beds' not found

---

    Code
      recipes_eval_select(data = Sacramento, info = info_sac)
    Condition
      Error in `recipes_eval_select()`:
      ! Argument `quos` is missing, with no default.

