# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_time()`:
      Caused by error in `bake()`:
      ! Name collision occured. The following variable names already exists:
      i  time_hour

# printing

    Code
      print(date_rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 1
      
      -- Operations 
      * Time features from: all_predictors()

---

    Code
      prep(date_rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 1
      
      -- Training information 
      Training data contained 5 data points and no incomplete rows.
      
      -- Operations 
      * Time features from: times | Trained

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
      * Time features from: <none>

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
      * Time features from: <none> | Trained

