# terms_select() is deprecated

    Code
      terms_select(info = info1, quos(all_predictors()))
    Condition
      Warning:
      `terms_select()` was deprecated in recipes 0.1.17.
      i Please use `recipes_eval_select()` instead.
      Warning:
      `with_handlers()` is deprecated as of rlang 1.0.0.
      i Please use `tryCatch()`, `withCallingHandlers()`, or `try_fetch()`.
    Output
      [1] "city"      "zip"       "beds"      "baths"     "sqft"      "type"     
      [7] "price"     "latitude"  "longitude"

# simple role selections

    Code
      terms_select(info = info1, quos(all_outcomes()))
    Condition
      Error:
      ! No variables or terms were selected.

# simple name selections

    Code
      terms_select(info = info1, quos(log(beds)))
    Condition
      Error in `FUN()`:
      ! Not all functions are allowed in step function selectors (e.g. `log`). See ?selections.

---

    Code
      terms_select(info = info1, quos(beds:sqft))
    Condition
      Error in `FUN()`:
      ! Not all functions are allowed in step function selectors (e.g. `:`). See ?selections.

---

    Code
      terms_select(info = info1, quos(I(beds:sqft)))
    Condition
      Error in `FUN()`:
      ! Not all functions are allowed in step function selectors (e.g. `I`, `:`). See ?selections.

---

    Code
      terms_select(info = info1, quos(matches("blahblahblah")))
    Condition
      Error:
      ! No variables or terms were selected.

