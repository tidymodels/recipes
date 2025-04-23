# is_unsupervised_weights works

    Code
      is_unsupervised_weights(3)
    Condition
      Error in `is_unsupervised_weights()`:
      ! Must be be a `case_weights` variable.

---

    Code
      too_many_case_weights(c("var1", "var2"))
    Condition
      Error:
      ! There should only be a single column with the role `case_weights`.
      i In these data, there are 2 columns: `var1` and `var2`.

# get_case_weights() catches non-numeric case weights

    Code
      prep(step_normalize(recipe(~., data = mtcars), all_predictors()))
    Condition
      Error in `step_normalize()`:
      Caused by error in `prep()`:
      x vs has a `case_weights` role and should be numeric, but is a string.
      i Only numeric case weights are supported in recipes.

