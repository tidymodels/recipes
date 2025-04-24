# wrong type

    Code
      prep(rec4, ex_dat, verbose = FALSE)
    Condition
      Error in `step_ratio()`:
      Caused by error in `prep()`:
      x All columns selected for the step should be double or integer.
      * 1 factor variable found: `x5`

---

    Code
      prep(rec5, ex_dat, verbose = FALSE)
    Condition
      Error in `step_ratio()`:
      Caused by error in `prep()`:
      x All columns selected for the step should be double or integer.
      * 1 factor variable found: `x5`

---

    Code
      prep(rec6, ex_dat, verbose = FALSE)
    Condition
      Error in `step_ratio()`:
      Caused by error in `prep()`:
      x All columns selected for the step should be double or integer.
      * 1 factor variable found: `x5`

# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_ratio()`:
      Caused by error in `bake()`:
      ! Name collision occurred. The following variable names already exist:
      * `mpg_o_disp`

# recipes_argument_select() is used

    Code
      prep(step_ratio(recipe(mpg ~ ., data = mtcars), disp, denom = NULL))
    Condition
      Error in `step_ratio()`:
      Caused by error in `prep()`:
      ! `denom` must not be `NULL`.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(rec1, ex_dat[, 2:5])
    Condition
      Error in `step_ratio()`:
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

# keep_original_cols - can prep recipes with it missing

    Code
      rec <- prep(rec)
    Condition
      Warning:
      `keep_original_cols` was added to `step_ratio()` after this recipe was created.
      i Regenerate your recipe to avoid this warning.

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 5
      
      -- Operations 
      * Ratios from: all_numeric() all_numeric()

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 5
      
      -- Training information 
      Training data contained 10 data points and 1 incomplete row.
      
      -- Operations 
      * Ratios from: x2, x3, x4, x1, x1, x2, x3, x4 | Trained

# bad args

    Code
      prep(step_ratio(recipe(~ mpg + disp, mtcars), mpg, denom = disp, naming = NULL))
    Condition
      Error in `step_ratio()`:
      Caused by error in `prep()`:
      ! `naming` must be a function, not `NULL`.

