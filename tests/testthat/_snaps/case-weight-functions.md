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

