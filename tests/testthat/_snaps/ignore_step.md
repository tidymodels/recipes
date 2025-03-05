# ignore_step() errors when needed

    Code
      ignore_step(rec)
    Condition
      Error in `ignore_step()`:
      ! `x` doesn't contain any steps to remove.

---

    Code
      ignore_step(rec1234)
    Condition
      Error in `ignore_step()`:
      ! One of `number` or `id` must be supplied.

---

    Code
      ignore_step(rec1234, number = 1, id = "pca")
    Condition
      Error in `ignore_step()`:
      ! Exactly one of `number` or `id` must be supplied.

---

    Code
      ignore_step(rec1234, number = 0)
    Condition
      Error in `ignore_step()`:
      ! `number` must only contain values between 1 and 4. Not 0.

---

    Code
      ignore_step(rec1234, number = 10)
    Condition
      Error in `ignore_step()`:
      ! `number` must only contain values between 1 and 4. Not 10.

---

    Code
      ignore_step(rec1234, id = "no id")
    Condition
      Error in `ignore_step()`:
      ! Supplied `id` ("no id") not found in the recipe.

---

    Code
      ignore_step(rec12)
    Condition
      Error in `ignore_step()`:
      ! `x` must not contain any trained steps.

---

    Code
      ignore_step(rec1234)
    Condition
      Error in `ignore_step()`:
      ! `x` must not contain any trained steps.

