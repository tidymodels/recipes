# bad args

    Code
      rec %>% step_string2factor(w, n) %>% prep(ex_dat)
    Condition
      Error in `step_string2factor()`:
      Caused by error in `prep()`:
      ! All columns selected for the step should be string, factor, or ordered.

---

    Code
      rec %>% step_string2factor(n, ordered = "yes") %>% prep(ex_dat)
    Condition
      Error in `step_string2factor()`:
      ! `ordered` should be a single logical variable

# printing

    Code
      print(ex_3)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 5
      
      -- Training information 
      Training data contained 200 data points and no incomplete rows.
      
      -- Operations 
      * Factor variables from: w, x | Trained

---

    Code
      prep(ex_3)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 5
      
      -- Training information 
      Training data contained 200 data points and no incomplete rows.
      
      -- Operations 
      * Factor variables from: w, x | Trained

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

