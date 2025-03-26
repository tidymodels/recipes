# bad args

    Code
      bake(rec, new_data = sacr_te, composition = "matrix")
    Condition
      Error in `bake()`:
      ! `data` must only contain numeric columns.
      i These columns aren't numeric: "beds" and "type".

---

    Code
      juice(rec, composition = "matrix")
    Condition
      Error in `juice()`:
      ! `data` must only contain numeric columns.
      i These columns aren't numeric: "beds" and "type".

