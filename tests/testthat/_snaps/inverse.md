# bake method errors when needed non-standard role columns are missing

    Code
      bake(rec_trained, new_data = ex_dat[, 1:3])
    Condition
      Error in `step_inverse()`:
      ! The following required column is missing from `new_data`: x4.

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
      * Inverse transformation on: <none>

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
      * Inverse transformation on: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 5
      
      -- Operations 
      * Inverse transformation on: x1, x2, x3, x4

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 5
      
      -- Training information 
      Training data contained 20 data points and no incomplete rows.
      
      -- Operations 
      * Inverse transformation on: x1, x2, x3, x4 | Trained

# bad args

    Code
      prep(step_inverse(recipe(~., data = ex_dat), x1, x2, x3, x4, offset = function(
        x) x / 3))
    Condition
      Error in `step_inverse()`:
      Caused by error in `prep()`:
      ! `offset` must be a number, not a function.

