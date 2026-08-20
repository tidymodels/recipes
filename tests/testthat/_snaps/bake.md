# bake() errors on bad stop_at

    Code
      bake(car_rec, mtcars, stop_at = 2)
    Condition
      Error in `bake()`:
      ! `stop_at` must be a step number between 1 and 1, not 2.

---

    Code
      bake(car_rec, mtcars, stop_at = 0)
    Condition
      Error in `bake()`:
      ! `stop_at` must be a step number between 1 and 1, not 0.

---

    Code
      bake(car_rec, mtcars, stop_at = 1.5)
    Condition
      Error in `bake()`:
      ! `stop_at` must be a whole step number, not 1.5.

---

    Code
      bake(car_rec, mtcars, stop_at = "nope")
    Condition
      Error in `bake()`:
      x `stop_at` must be a single step number or step id, not "nope".
      i The step ids of `object` are "center".

---

    Code
      bake(car_rec, mtcars, stop_at = TRUE)
    Condition
      Error in `bake()`:
      ! `stop_at` must be a single step number or step id, not `TRUE`.

---

    Code
      bake(car_rec, mtcars, stop_at = c(1, 1))
    Condition
      Error in `bake()`:
      ! `stop_at` must be a single step number or step id, not a double vector.

---

    Code
      bake(car_rec, mtcars, stop_at = NA)
    Condition
      Error in `bake()`:
      ! `stop_at` must be a single step number or step id, not `NA`.

---

    Code
      bake(car_rec, mtcars, stop_at = integer(0))
    Condition
      Error in `bake()`:
      ! `stop_at` must be a single step number or step id, not an empty integer vector.

---

    Code
      bake(car_rec, new_data = NULL, stop_at = 1)
    Condition
      Error in `bake()`:
      x `stop_at` cannot be used with `new_data = NULL`.
      i Intermediate versions of the training set are not retained by `prep()`; pass the training data to `new_data` instead.

---

    Code
      bake(prep(recipe(cyl ~ ., mtcars)), mtcars, stop_at = 1)
    Condition
      Error in `bake()`:
      ! `stop_at` cannot be used with a recipe that has no steps.

