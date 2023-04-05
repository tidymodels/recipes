# wrong type

    Code
      prep(rec4, ex_dat, verbose = FALSE)
    Condition
      Error in `step_ratio()`:
      Caused by error in `prep()`:
      ! All columns selected for the step should be double, or integer.

---

    Code
      prep(rec5, ex_dat, verbose = FALSE)
    Condition
      Error in `step_ratio()`:
      Caused by error in `prep()`:
      ! All columns selected for the step should be double, or integer.

---

    Code
      prep(rec6, ex_dat, verbose = FALSE)
    Condition
      Error in `step_ratio()`:
      Caused by error in `prep()`:
      ! All columns selected for the step should be double, or integer.

# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_ratio()`:
      Caused by error in `bake()`:
      ! Name collision occured. The following variable names already exists:
      i  mpg_o_disp

# printing

    Code
      print(rec3)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 5
      
      -- Operations 
      * Ratios from: all_numeric(), all_numeric()

---

    Code
      prep(rec3)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 5
      
      -- Training information 
      Training data contained 10 data points and 1 incomplete row.
      
      -- Operations 
      * Ratios from: x2, x3, x4, x1, x1, x2, x3, x4 | Trained

# can prep recipes with no keep_original_cols

    Code
      prep1 <- prep(rec1, training = ex_dat, verbose = FALSE)
    Condition
      Warning:
      'keep_original_cols' was added to `step_ratio()` after this recipe was created.
      Regenerate your recipe to avoid this warning.

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
      * Ratios from: mpg

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
      * Ratios from: <none> | Trained

