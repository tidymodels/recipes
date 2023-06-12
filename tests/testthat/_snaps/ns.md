# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_ns()`:
      Caused by error in `bake()`:
      ! Name collision occured. The following variable names already exists:
      i  mpg_ns_1

# can prep recipes with no keep_original_cols

    Code
      rec <- prep(rec)
    Condition
      Warning:
      'keep_original_cols' was added to `step_ns()` after this recipe was created.
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
      * Natural splines on: <none>

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
      * Natural splines on: <none> | Trained

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
      * Natural splines on: disp, wt

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
      * Natural splines on: disp, wt | Trained

