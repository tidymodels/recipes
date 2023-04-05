# printing

    Code
      print(date_rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 2
      
      -- Operations 
      * Date features from: all_predictors()

---

    Code
      prep(date_rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 2
      
      -- Training information 
      Training data contained 10 data points and no incomplete rows.
      
      -- Operations 
      * Date features from: Dan, Stefan | Trained

# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_date()`:
      Caused by error in `bake()`:
      ! Name collision occured. The following variable names already exists:
      i  Dan_year

# can prep recipes with no keep_original_cols

    Code
      date_rec <- prep(date_rec, training = examples, verbose = FALSE)
    Condition
      Warning:
      'keep_original_cols' was added to `step_date()` after this recipe was created.
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
      * Date features from: <none>

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
      * Date features from: <none> | Trained

