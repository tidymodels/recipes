# bad args

    Code
      prep(step_factor2string(recipe(~., data = ex_dat, strings_as_factors = FALSE),
      w, x), ex_dat)
    Condition
      Error in `step_factor2string()`:
      Caused by error in `prep()`:
      x All columns selected for the step should be factor or ordered.
      * 2 string variables found: `w` and `x`

# bake method errors when needed non-standard role columns are missing

    Code
      bake(ex_1, new_data = ex_dat[, 1:3])
    Condition
      Error in `step_factor2string()`:
      ! The following required column is missing from `new_data`: z.

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
      * Character variables from: <none>

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
      * Character variables from: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 4
      
      -- Operations 
      * Character variables from: y z

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 4
      
      -- Training information 
      Training data contained 200 data points and no incomplete rows.
      
      -- Operations 
      * Character variables from: y z | Trained

