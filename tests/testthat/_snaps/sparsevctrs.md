# recipe() errors if sparse matrix has no colnames

    Code
      recipe(~., data = hotel_data)
    Condition
      Error in `recipe()`:
      ! `x` must have column names.

---

    Code
      recipe(hotel_data)
    Condition
      Error in `recipe()`:
      ! `x` must have column names.

