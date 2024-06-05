# recipes_ptype errors on old recipes

    Code
      recipes_ptype(rec)
    Condition
      Error in `recipes_ptype()`:
      x Doesn't work on recipes created prior to version 1.1.0.
      i Please recreate recipe.

# recipes_ptype_validate() works

    Code
      recipes_ptype_validate(rec, data_new)
    Condition
      Error:
      ! Not all variables in the recipe are present in the supplied training set: `id`.

---

    Code
      recipes_ptype_validate(rec, data_new)
    Condition
      Error:
      ! Not all variables in the recipe are present in the supplied training set: `id` and `x1`.

---

    Code
      recipes_ptype_validate(rec, data_new)
    Condition
      Error:
      x The following variable has the wrong class:
      * `id` must have class <numeric> not <integer>.

---

    Code
      recipes_ptype_validate(rec, data_new)
    Condition
      Error:
      x The following variables has the wrong class:
      * `id` must have class <numeric> not <integer>.
      * `x2` must have class <integer> not <factor>.
      * `y` must have class <numeric> not <integer>.

---

    Code
      recipes_ptype_validate(rec, data_new)
    Condition
      Error:
      x The following variable has the wrong attributed: `x1`.

---

    Code
      recipes_ptype_validate(rec, data_new)
    Condition
      Error:
      x The following variables has the wrong attributed: `id` and `x1`.

