# harmonic error

    Code
      step_harmonic(recipe(osc ~ time_var, data = harmonic_dat), time_var, frequency = 1,
      cycle_size = NA)
    Condition
      Error in `step_harmonic()`:
      x `cycle_size` must have at least one non-NA numeric value.
      i It was `NA`.

---

    Code
      step_harmonic(recipe(osc ~ time_var, data = harmonic_dat), time_var, frequency = 1,
      starting_val = 0, cycle_size = NA)
    Condition
      Error in `step_harmonic()`:
      x `cycle_size` must have at least one non-NA numeric value.
      i It was `NA`.

---

    Code
      step_harmonic(recipe(osc ~ time_var, data = harmonic_dat), time_var, frequency = 1,
      starting_val = 0, cycle_size = "a")
    Condition
      Error in `step_harmonic()`:
      x `cycle_size` must have at least one non-NA numeric value.
      i It was a string.

---

    Code
      step_harmonic(recipe(osc ~ time_var, data = harmonic_dat), time_var, frequency = 1,
      starting_val = "a", cycle_size = 86400)
    Condition
      Error in `step_harmonic()`:
      ! starting_val must be NA, numeric, Date or POSIXt, not a string.

---

    Code
      step_harmonic(recipe(osc ~ time_var, data = harmonic_dat), time_var, frequency = 1,
      starting_val = factor("a"), cycle_size = 86400)
    Condition
      Error in `step_harmonic()`:
      ! starting_val must be NA, numeric, Date or POSIXt, not a <factor> object.

# harmonic NA in term

    Code
      bake(prep(step_harmonic(recipe(osc ~ time_var, data = harmonic_dat), time_var,
      frequency = 4, cycle_size = 86400)), new_data = NULL)
    Condition
      Error in `step_harmonic()`:
      Caused by error in `bake()`:
      ! Variable must have at least one non-NA value.

# harmonic character in term

    Code
      bake(prep(step_harmonic(recipe(osc ~ time_var, data = harmonic_dat), time_var,
      frequency = 4, cycle_size = 86400)), new_data = NULL)
    Condition
      Error in `step_harmonic()`:
      Caused by error in `prep()`:
      x All columns selected for the step should be date, datetime, or numeric.
      * 1 factor variable found: `time_var`

# harmonic cycle_size length

    Code
      prep(step_harmonic(recipe(osc ~ time_var_1 + time_var_2 + time_var_3, data = harmonic_dat),
      time_var_1, time_var_2, time_var_3, frequency = 4, cycle_size = c(86400, 86400)))
    Condition
      Error in `step_harmonic()`:
      Caused by error in `prep()`:
      ! `cycle_size` must be length 1 or the same length as the input columns.

# harmonic starting_val length

    Code
      prep(step_harmonic(recipe(osc ~ time_var_1 + time_var_2 + time_var_3, data = harmonic_dat),
      time_var_1, time_var_2, time_var_3, frequency = 4, starting_val = c(86400,
        86400), cycle_size = 86400))
    Condition
      Error in `step_harmonic()`:
      Caused by error in `prep()`:
      ! `starting_val` must be length 1 or the same length as the input columns.

# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_harmonic()`:
      Caused by error in `bake()`:
      ! Name collision occurred. The following variable names already exist:
      * `mpg_sin_1`

# bake method errors when needed non-standard role columns are missing

    Code
      bake(rec, new_data = harmonic_dat_mult[, 1:2])
    Condition
      Error in `step_harmonic()`:
      ! The following required column is missing from `new_data`: time_var_2.

# empty printing

    Code
      rec
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 10
      
      -- Operations 
      * Harmonic numeric variables for: <none>

---

    Code
      rec
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 10
      
      -- Training information 
      Training data contained 32 data points and no incomplete rows.
      
      -- Operations 
      * Harmonic numeric variables for: <none> | Trained

# keep_original_cols - can prep recipes with it missing

    Code
      rec <- prep(rec)
    Condition
      Warning:
      `keep_original_cols` was added to `step_harmonic()` after this recipe was created.
      i Regenerate your recipe to avoid this warning.

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 10
      
      -- Operations 
      * Harmonic numeric variables for: hp

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 10
      
      -- Training information 
      Training data contained 32 data points and no incomplete rows.
      
      -- Operations 
      * Harmonic numeric variables for: hp | Trained

