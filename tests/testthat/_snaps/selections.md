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

# error when selecting case weights

    Code
      prep(step_normalize(recipe(~., data = mtcars), hp))
    Condition
      Error in `step_normalize()`:
      Caused by error in `prep()`:
      ! Cannot select case weights variable.

