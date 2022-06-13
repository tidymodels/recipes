# trained

    Code
      trained <- prep(Sacramento_rec, training = Sacramento)

# bad args

    Code
      tidy(trained, number = NULL)
    Condition
      Error in `tidy()`:
      ! object 'trained' not found

---

    Code
      tidy(trained, number = 100)
    Condition
      Error in `tidy()`:
      ! object 'trained' not found

---

    Code
      tidy(trained, number = 1, id = "id")
    Condition
      Error in `tidy()`:
      ! object 'trained' not found

---

    Code
      tidy(trained, id = "id")
    Condition
      Error in `tidy()`:
      ! object 'trained' not found

