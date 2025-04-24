# signed arg

    Code
      prep(rec2)
    Condition
      Warning:
      When `signed` is TRUE, `offset` will be ignored.
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 1
      
      -- Training information 
      Training data contained 4 data points and no incomplete rows.
      
      -- Operations 
      * Signed log transformation on: x | Trained

# bake method errors when needed non-standard role columns are missing

    Code
      bake(rec_trained, new_data = ex_dat[, 1:3])
    Condition
      Error in `step_log()`:
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
      * Log transformation on: <none>

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
      * Log transformation on: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 4
      
      -- Operations 
      * Log transformation on: x1, x2, x3, x4

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 4
      
      -- Training information 
      Training data contained 20 data points and no incomplete rows.
      
      -- Operations 
      * Log transformation on: x1, x2, x3, x4 | Trained

# bad args

    Code
      prep(step_log(recipe(~., data = ex_dat), x1, base = -1))
    Condition
      Error in `step_log()`:
      Caused by error in `prep()`:
      ! `base` must be a number larger than or equal to 0, not the number -1.

---

    Code
      prep(step_log(recipe(~., data = ex_dat), x1, offset = "none"))
    Condition
      Error in `step_log()`:
      Caused by error in `prep()`:
      ! `offset` must be a number, not the string "none".

---

    Code
      prep(step_log(recipe(~., data = ex_dat), x1, signed = "yes"))
    Condition
      Error in `step_log()`:
      Caused by error in `prep()`:
      ! `signed` must be `TRUE` or `FALSE`, not the string "yes".

