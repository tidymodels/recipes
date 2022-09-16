# bad args

    Code
      bake(rec, new_data = sacr_te, composition = "dgCMatrix")
    Condition
      Error in `convert_matrix()`:
      ! Columns (`beds`, `type`) are not numeric; cannot convert to matrix.

---

    Code
      juice(rec, composition = "dgCMatrix")
    Condition
      Error in `convert_matrix()`:
      ! Columns (`beds`, `type`) are not numeric; cannot convert to matrix.

---

    Code
      recipe(~., data = ames) %>% prep() %>% bake(new_data = NULL, composition = "dgCMatrix")
    Condition
      Error in `convert_matrix()`:
      ! 40 columns are not numeric; cannot convert to matrix.

