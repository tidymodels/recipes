# bad args

    Code
      prep(step_string2factor(rec, w, n), ex_dat)
    Condition
      Error in `step_string2factor()`:
      Caused by error in `prep()`:
      x All columns selected for the step should be string, factor, or ordered.
      * 1 integer variable found: `n`

---

    Code
      prep(step_string2factor(rec, w, x, ordered = "yes"), ex_dat)
    Condition
      Error in `step_string2factor()`:
      Caused by error in `prep()`:
      ! `ordered` must be `TRUE` or `FALSE`, not the string "yes".

# bake method errors when needed non-standard role columns are missing

    Code
      bake(rec_trained, new_data = ex_dat[, -1])
    Condition
      Error in `step_string2factor()`:
      ! The following required column is missing from `new_data`: w.

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
      * Factor variables from: <none>

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
      * Factor variables from: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 5
      
      -- Operations 
      * Factor variables from: w x

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 5
      
      -- Training information 
      Training data contained 200 data points and no incomplete rows.
      
      -- Operations 
      * Factor variables from: w x | Trained

