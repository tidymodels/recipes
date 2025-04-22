# out of bounds logit trans

    Code
      prep(rec, training = ex_dat, verbose = FALSE)
    Condition
      Error in `step_logit()`:
      Caused by error in `binomial()$linkfun()`:
      ! Value -0.77772 out of range (0, 1)

# bake method errors when needed non-standard role columns are missing

    Code
      bake(rec_trained, new_data = ex_dat[, 2:3])
    Condition
      Error in `step_logit()`:
      ! The following required column is missing from `new_data`: x1.

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
      * Logit transformation on: <none>

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
      * Logit transformation on: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 3
      
      -- Operations 
      * Logit transformation on: x1

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 3
      
      -- Training information 
      Training data contained 20 data points and no incomplete rows.
      
      -- Operations 
      * Logit transformation on: x1 | Trained

# bad args

    Code
      prep(step_logit(recipe(~., data = ex_dat), x1, offset = "sure"))
    Condition
      Error in `step_logit()`:
      Caused by error in `prep()`:
      ! `offset` must be a number, not the string "sure".

