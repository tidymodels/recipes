# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_bs()`:
      Caused by error in `bake()`:
      ! Name collision occured. The following variable names already exists:
      i  mpg_bs_1

# printing

    Code
      print(with_bs)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 5
      
      -- Operations 
      * B-splines on: carbon, hydrogen

---

    Code
      prep(with_bs)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 5
      
      -- Training information 
      Training data contained 456 data points and no incomplete rows.
      
      -- Operations 
      * B-splines on: carbon, hydrogen | Trained

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
      * B-splines on: <none>

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
      * B-splines on: <none> | Trained

