# trained

    Code
      trained <- prep(Sacramento_rec, training = Sacramento)

# bad args

    Code
      tidy(trained, number = NULL)
    Condition
      Error in `tidy()`:
      ! `number` must be a whole number or `NA`, not `NULL`.

---

    Code
      tidy(trained, number = 100)
    Condition
      Error in `tidy()`:
      ! `number` should be a single value between 1 and 4, not a number.

---

    Code
      tidy(trained, number = 1, id = "id")
    Condition
      Error in `tidy()`:
      ! You may specify `number` or `id`, but not both.

---

    Code
      tidy(trained, id = "id")
    Condition
      Error in `tidy()`:
      ! `id` must be one of "other_MvKZG", "center_p66OR", "dummy_5eKj5", or "cols_z8tZQ", not "id".

---

    Code
      tidy(trained, id = c("id", "id2"))
    Condition
      Error in `tidy()`:
      ! `id` must be a single string or `NA`, not a character vector.

# bag args

    Code
      tidy(single_step)
    Condition
      Error in `tidy()`:
      ! No <tidy> method for a step with classes: <step_notidy/step>.

---

    Code
      tidy(single_check)
    Condition
      Error in `tidy()`:
      ! No <tidy> method for a check with classes: <check_notidy/check>.

