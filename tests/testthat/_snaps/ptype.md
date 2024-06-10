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
      * `id` must have class <numeric>, not <integer>.

---

    Code
      recipes_ptype_validate(rec, data_new)
    Condition
      Error:
      x The following variables have the wrong class:
      * `id` must have class <numeric>, not <integer>.
      * `x2` must have class <integer>, not <factor>.
      * `y` must have class <numeric>, not <integer>.

---

    Code
      recipes_ptype_validate(rec, data_new)
    Condition
      Error:
      x The following variable has the wrong attributes: `x1`.
      Run `lapply(recipes_ptype(rec), attributes)` to see expected attributes. For `rec` being the name of your recipe.

---

    Code
      recipes_ptype_validate(rec, data_new)
    Condition
      Error:
      x The following variables have the wrong attributes: `id` and `x1`.
      Run `lapply(recipes_ptype(rec), attributes)` to see expected attributes. For `rec` being the name of your recipe.

---

    Code
      recipes_ptype_validate(rec, data_new)
    Condition
      Error:
      x The following variable has the wrong attributes: `x2`.
      * The factor levels of `x2` don't match.
      Run `lapply(recipes_ptype(rec), attributes)` to see expected attributes. For `rec` being the name of your recipe.

