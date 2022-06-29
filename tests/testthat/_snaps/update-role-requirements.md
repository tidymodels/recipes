# `role` is validated

    Code
      update_role_requirements(rec, 1)
    Condition
      Error in `update_role_requirements()`:
      ! Can't convert `role` <double> to <character>.

---

    Code
      update_role_requirements(rec, c("x", "y"))
    Condition
      Error in `update_role_requirements()`:
      ! `role` must have size 1, not size 2.

# can't update a role that doesn't exist

    Code
      update_role_requirements(rec, "id", bake = FALSE)
    Condition
      Error in `update_role_requirements()`:
      ! `role` must be a preexisting role in the recipe.
      i "id" is not a preexisting role.

# can't update the predictor role

    Code
      update_role_requirements(rec, "predictor", bake = FALSE)
    Condition
      Error in `update_role_requirements()`:
      ! Can't update the `bake` requirement of the "predictor" role.
      i The "predictor" role is always required at `bake()` time.

# can't update the outcome role

    Code
      update_role_requirements(rec, "outcome", bake = FALSE)
    Condition
      Error in `update_role_requirements()`:
      ! Can't update the `bake` requirement of the "outcome" role.
      i The "outcome" role is never required at `bake()` time.

# can update `bake` requirements after prepping

    Code
      bake(rec, df)
    Condition
      Error in `bake()`:
      ! The following required columns are missing from `new_data`: "x".
      i These columns have one of the following roles, which are required at `bake()` time: "id".
      i If these roles are not required at `bake()` time, use `update_role_requirements(role = "your_role", bake = FALSE)`.

# errors on missing 'predictor's

    Code
      bake(rec, df)
    Condition
      Error in `bake()`:
      ! The following required columns are missing from `new_data`: "x", "z", "w".
      i These columns have one of the following roles, which are required at `bake()` time: "predictor".

# can request that case weights be required

    Code
      bake(rec, df)
    Condition
      Error in `bake()`:
      ! The following required columns are missing from `new_data`: "w".
      i These columns have one of the following roles, which are required at `bake()` time: "case_weights".
      i If these roles are not required at `bake()` time, use `update_role_requirements(role = "your_role", bake = FALSE)`.

# nonstandard roles are required by default

    Code
      bake(rec, df)
    Condition
      Error in `bake()`:
      ! The following required columns are missing from `new_data`: "x", "z".
      i These columns have one of the following roles, which are required at `bake()` time: "id".
      i If these roles are not required at `bake()` time, use `update_role_requirements(role = "your_role", bake = FALSE)`.

# unspecified roles are required by default

    Code
      bake(rec, df)
    Condition
      Error in `bake()`:
      ! The following required columns are missing from `new_data`: "x", "z".
      i These columns have one of the following roles, which are required at `bake()` time: "NA".
      i If these roles are not required at `bake()` time, use `update_role_requirements(role = "your_role", bake = FALSE)`.

# can bake on an old recipe that doesn't have `requirements`

    Code
      bake(rec, df)
    Condition
      Error in `bake()`:
      ! The following required columns are missing from `new_data`: "x".
      i These columns have one of the following roles, which are required at `bake()` time: "predictor".

# can bake on an old recipe that doesn't have `requirements$bake`

    Code
      bake(rec, df)
    Condition
      Error in `bake()`:
      ! The following required columns are missing from `new_data`: "x".
      i These columns have one of the following roles, which are required at `bake()` time: "predictor".

# can update the role requirements of an old recipe

    Code
      bake(rec, df)
    Condition
      Error in `bake()`:
      ! The following required columns are missing from `new_data`: "x".
      i These columns have one of the following roles, which are required at `bake()` time: "id".
      i If these roles are not required at `bake()` time, use `update_role_requirements(role = "your_role", bake = FALSE)`.

# `bake` is validated

    Code
      update_role_requirements(rec, "id", bake = 1)
    Condition
      Error in `update_role_requirements()`:
      ! `bake` must be a single `TRUE` or `FALSE`.

---

    Code
      update_role_requirements(rec, "id", bake = c(TRUE, FALSE))
    Condition
      Error in `update_role_requirements()`:
      ! `bake` must be a single `TRUE` or `FALSE`.

---

    Code
      update_role_requirements(rec, "id", bake = NA)
    Condition
      Error in `update_role_requirements()`:
      ! `bake` must be a single `TRUE` or `FALSE`.

