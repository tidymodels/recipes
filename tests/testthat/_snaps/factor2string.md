# bad args

    Code
      rec %>% step_factor2string(w, x) %>% prep(ex_dat, strings_as_factors = FALSE)
    Condition
      Error in `step_factor2string()`:
      Caused by error in `prep()`:
      ! All columns selected for the step should be factor, or ordered.

# printing

    Code
      print(ex_3)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 4
      
      -- Training information 
      Training data contained 200 data points and no incomplete rows.
      
      -- Operations 
      * Character variables from: y, z | Trained

---

    Code
      prep(ex_3)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 4
      
      -- Training information 
      Training data contained 200 data points and no incomplete rows.
      
      -- Operations 
      * Character variables from: y, z | Trained

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

