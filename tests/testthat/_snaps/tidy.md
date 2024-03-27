# trained

    Code
      trained <- prep(Sacramento_rec, training = Sacramento)

# bad args

    Code
      tidy(trained, number = NULL)
    Condition
      Error in `tidy()`:
      ! If `number` is provided, it must be a length 1 integer vector.

---

    Code
      tidy(trained, number = 100)
    Condition
      Error in `tidy()`:
      ! `number` should be a single value between 1 and 4, not 100.

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
      ! Supplied `id` ("id") not found in the recipe.

---

    Code
      tidy(trained, id = c("id", "id2"))
    Condition
      Error in `tidy()`:
      ! If `id` is provided, it must be a length 1 character vector.

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

