# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_indicate_na()`:
      Caused by error in `bake()`:
      ! Name collision occurred. The following variable names already exist:
      * `na_ind_mpg`

# bake method errors when needed non-standard role columns are missing

    Code
      bake(rec1, new_data = test[, 2:3])
    Condition
      Error in `step_indicate_na()`:
      ! The following required column is missing from `new_data`: col1.

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
      * Creating missing data variable indicators for: <none>

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
      * Creating missing data variable indicators for: <none> | Trained

# keep_original_cols - can prep recipes with it missing

    Code
      rec <- prep(rec)
    Condition
      Warning:
      `keep_original_cols` was added to `step_indicate_na()` after this recipe was created.
      i Regenerate your recipe to avoid this warning.

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 5
      
      -- Operations 
      * Creating missing data variable indicators for: all_predictors()

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 5
      
      -- Training information 
      Training data contained 153 data points and 42 incomplete rows.
      
      -- Operations 
      * Creating missing data variable indicators for: Solar.R, ... | Trained

