# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_ns()`:
      Caused by error in `bake()`:
      ! Name collision occured. The following variable names already exists:
      i  mpg_ns_1

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
      * Natural splines on: carbon, hydrogen

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
      * Natural splines on: carbon, hydrogen | Trained

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

