# error checks

    Code
      rec %>% step_window(y1, size = 6) %>% prep()
    Condition
      Error in `step_window()`:
      Caused by error in `prep()`:
      ! `size` should be odd, not 6.

---

    Code
      rec %>% step_window(y1, size = NA) %>% prep()
    Condition
      Error in `step_window()`:
      Caused by error in `prep()`:
      ! `size` must be a whole number, not `NA`.

---

    Code
      rec %>% step_window(y1, size = NULL) %>% prep()
    Condition
      Error in `step_window()`:
      Caused by error in `prep()`:
      ! `size` must be a whole number, not `NULL`.

---

    Code
      rec %>% step_window(y1, statistic = "average")
    Condition
      Error in `step_window()`:
      ! `statistic` must be one of "mean", "median", "sd", "var", "sum", "prod", "min", or "max", not "average".

---

    Code
      rec %>% step_window(y1, size = 1) %>% prep()
    Condition
      Error in `step_window()`:
      Caused by error in `prep()`:
      ! `size` must be a whole number larger than or equal to 3, not the number 1.

---

    Code
      rec %>% step_window(y1, size = 2) %>% prep()
    Condition
      Error in `step_window()`:
      Caused by error in `prep()`:
      ! `size` must be a whole number larger than or equal to 3, not the number 2.

---

    Code
      rec %>% step_window(y1, size = -1) %>% prep()
    Condition
      Error in `step_window()`:
      Caused by error in `prep()`:
      ! `size` must be a whole number larger than or equal to 3, not the number -1.

---

    Code
      rec %>% step_window(y1, size = 3 + .Machine$double.eps) %>% prep()
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 6
      
      -- Training information 
      Training data contained 81 data points and no incomplete rows.
      
      -- Operations 
      * Moving 3-point mean on: y1 | Trained

---

    Code
      rec %>% step_window(y1, size = 3 + 2 * .Machine$double.eps) %>% prep()
    Condition
      Error in `step_window()`:
      Caused by error in `prep()`:
      ! `size` must be a whole number, not the number 3.

---

    Code
      prep(rec %>% step_window(fac), training = sim_dat)
    Condition
      Error in `step_window()`:
      Caused by error in `prep()`:
      x All columns selected for the step should be double or integer.
      * 1 factor variable found: `fac`

---

    Code
      prep(rec %>% step_window(y1, size = 1000L), training = sim_dat) %>% prep()
    Condition
      Error in `step_window()`:
      Caused by error in `prep()`:
      ! `size` should be odd, not 1000.

---

    Code
      prep(bad_names, training = sim_dat)
    Condition
      Error in `step_window()`:
      Caused by error in `prep()`:
      ! There were 2 terms selected but 1 value for the new features was passed to `names`.

# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_window()`:
      Caused by error in `bake()`:
      ! Name collision occurred. The following variable names already exist:
      * `new_value`

# error on too large window size

    Code
      recipe(~., data = mtcars) %>% step_window(mpg, size = 999) %>% prep()
    Condition
      Error in `step_window()`:
      Caused by error in `roller()`:
      ! The window is too large.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(rec_trained, new_data = sim_dat[, -1])
    Condition
      Error in `step_window()`:
      ! The following required column is missing from `new_data`: x1.

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
      * Moving 3-point mean on: <none>

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
      * Moving 3-point mean on: <none> | Trained

# keep_original_cols - can prep recipes with it missing

    Code
      rec <- prep(rec)
    Condition
      Warning:
      `keep_original_cols` was added to `step_window()` after this recipe was created.
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
      * Moving 3-point mean on: <none>

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
      * Moving 3-point mean on: <none> | Trained

