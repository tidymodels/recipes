# check_new_data works

    Code
      bake(log_obj, examples[, 1:3, drop = FALSE])
    Condition
      Error in `step_log()`:
      ! The following required column is missing from `new_data` in step 'log_IhS7o': V4.

---

    Code
      bake(log_obj, examples[, 1:2, drop = FALSE])
    Condition
      Error in `step_log()`:
      ! The following required columns are missing from `new_data` in step 'log_IhS7o': V3 and V4.

---

    Code
      bake(log_obj, examples[, 1, drop = FALSE])
    Condition
      Error in `step_log()`:
      ! The following required columns are missing from `new_data` in step 'log_IhS7o': V2, V3, and V4.

