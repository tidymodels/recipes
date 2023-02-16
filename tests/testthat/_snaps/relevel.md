# bad args

    Code
      rec %>% step_relevel(sqft, ref_level = 23) %>% prep()
    Condition
      Error in `step_relevel()`:
      Caused by error in `prep()`:
      ! All columns selected for the step should be string, factor, or ordered.

---

    Code
      rec %>% step_relevel(city, ref_level = "missing_level") %>% prep()
    Condition
      Error in `step_relevel()`:
      Caused by error in `prep()`:
      ! Columns must contain the reference level 'missing_level': city

# printing

    Code
      print(rec %>% step_relevel(zip, ref_level = "z95838"))
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 9
      
      -- Operations 
      * Re-order factor level to ref_level for: zip

---

    Code
      print(rec %>% step_relevel(zip, ref_level = "z95838") %>% prep())
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 9
      
      -- Training information 
      Training data contained 800 data points and no incomplete rows.
      
      -- Operations 
      * Re-order factor level to ref_level for: zip | Trained

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
      * Re-order factor level to ref_level for: <none>

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
      * Re-order factor level to ref_level for: <none> | Trained

