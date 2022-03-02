# trained

    Code
      trained <- prep(okc_rec, training = okc)
    Condition
      Warning:
      There are new levels in a factor: NA

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

