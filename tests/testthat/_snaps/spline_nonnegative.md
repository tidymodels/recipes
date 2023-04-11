# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_spline_nonnegative()`:
      Caused by error in `bake()`:
      ! Name collision occured. The following variable names already exists:
      i  mpg_01

# printing

    Code
      print(with_ns)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 5
      
      -- Operations 
      * Non-negative spline expansion: carbon, hydrogen

---

    Code
      prep(with_ns)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 5
      
      -- Training information 
      Training data contained 456 data points and no incomplete rows.
      
      -- Operations 
      * Non-negative spline expansion: carbon, hydrogen | Trained

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
      * Non-negative spline expansion: <none>

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
      * Non-negative spline expansion: <none> | Trained

