# terms_select() is deprecated

    Code
      terms_select(info = info1, quos(all_predictors()))
    Condition
      Warning:
      `terms_select()` was deprecated in recipes 0.1.17.
      Please use `recipes_eval_select()` instead.
    Output
      [1] "age"      "diet"     "height"   "location" "date"     "Class"   

# simple role selections

    Code
      terms_select(info = info1, quos(all_outcomes()))
    Condition
      Error:
      ! No variables or terms were selected.

# simple name selections

    Code
      terms_select(info = info1, quos(log(date)))
    Condition
      Error in `FUN()`:
      ! Not all functions are allowed in step function selectors (e.g. `log`). See ?selections.

---

    Code
      terms_select(info = info1, quos(date:age))
    Condition
      Error in `FUN()`:
      ! Not all functions are allowed in step function selectors (e.g. `:`). See ?selections.

---

    Code
      terms_select(info = info1, quos(I(date:age)))
    Condition
      Error in `FUN()`:
      ! Not all functions are allowed in step function selectors (e.g. `I`, `:`). See ?selections.

---

    Code
      terms_select(info = info1, quos(matches("blahblahblah")))
    Condition
      Error:
      ! No variables or terms were selected.

---

    Code
      terms_select(info = info1)
    Condition
      Error in `is_empty()`:
      ! argument "terms" is missing, with no default

