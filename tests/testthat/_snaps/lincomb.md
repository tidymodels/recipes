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
      * Linear combination filter on: <none>

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
      * Linear combination filter removed: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 12
      
      -- Operations 
      * Linear combination filter on: all_predictors()

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 12
      
      -- Training information 
      Training data contained 24 data points and no incomplete rows.
      
      -- Operations 
      * Linear combination filter removed: N1, P1, K1 | Trained

# bad args

    Code
      prep(step_lincomb(dum_rec, all_predictors(), max_steps = 0))
    Condition
      Error in `step_lincomb()`:
      Caused by error in `prep()`:
      ! `max_steps` must be a whole number larger than or equal to 1, not the number 0.

