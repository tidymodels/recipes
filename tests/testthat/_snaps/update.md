# cannot create new fields for a step

    Code
      update(stp, y = 5)
    Condition
      Error in `update()`:
      ! The step you are trying to update, `step_stp()`, does not have the y field.

# cannot update trained steps

    Code
      update(stp, x = 5)
    Condition
      Error in `update()`:
      ! To update `step_stp()`, it must not be trained.

# update() errors on duplicate assignments

    Code
      update(step, x = 5, x = 6)
    Condition
      Error in `update()`:
      ! To update `step_stp()`, it must not be trained.

