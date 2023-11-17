# error checks

    Code
      rec %>% step_window(y1, size = 6)
    Condition
      Error in `step_window()`:
      ! `size` should be odd, not 6.

---

    Code
      rec %>% step_window(y1, size = NA)
    Condition
      Error in `step_window()`:
      ! `size` must be a number, not `NA`.

---

    Code
      rec %>% step_window(y1, statistic = "average")
    Condition
      Error in `step_window()`:
      ! `statistic` should be one of: "mean", "median", "sd", "var", "sum", "prod", "min", or "max".

---

    Code
      rec %>% step_window(y1, size = 1)
    Condition
      Error in `step_window()`:
      ! `size` should be at least 3, not 1.

---

    Code
      rec %>% step_window(y1, size = 2)
    Condition
      Error in `step_window()`:
      ! `size` should be odd, not 2.

---

    Code
      rec %>% step_window(y1, size = -1)
    Condition
      Error in `step_window()`:
      ! `size` must be a number larger than or equal to 0, not the number -1.

---

    Code
      rec %>% step_window(y1, size = pi)
    Condition
      Warning:
      `size` was not an integer (3.14159265358979) and was converted to 3.
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 6
      
      -- Operations 
      * Moving 3-point mean on: y1

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
      prep(rec %>% step_window(y1, size = 1000L), training = sim_dat)
    Condition
      Error in `step_window()`:
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

