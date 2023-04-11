# printing

    Code
      print(holiday_rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 1
      
      -- Operations 
      * Holiday features from: all_predictors()

---

    Code
      prep(holiday_rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 1
      
      -- Training information 
      Training data contained 366 data points and 1 incomplete row.
      
      -- Operations 
      * Holiday features from: day | Trained

# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_holiday()`:
      Caused by error in `bake()`:
      ! Name collision occured. The following variable names already exists:
      i  day_Easter

# can prep recipes with no keep_original_cols

    Code
      holiday_rec <- prep(holiday_rec, training = test_data, verbose = FALSE)
    Condition
      Warning:
      'keep_original_cols' was added to `step_holiday()` after this recipe was created.
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
      * Holiday features from: <none>

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
      * Holiday features from: <none> | Trained

