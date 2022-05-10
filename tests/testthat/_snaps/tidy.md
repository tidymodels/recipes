# trained

    Code
      trained <- prep(Sacramento_rec, training = Sacramento)

# bad args

    Code
      tidy(trained, number = NULL)
    Condition
      Error in `if (is.na(number)) ...`:
      ! argument is of length zero

---

    Code
      tidy(trained, number = 100)
    Condition
      Error in `tidy()`:
      ! `number` should be a single value between 1 and 4.

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
      ! Supplied `id` not found in the recipe.

---

    Code
      tidy(trained, id = c("id", "id2"))
    Condition
      Error in `tidy()`:
      ! If `id` is provided, it must be a length 1 character vector.

