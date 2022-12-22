# bad selector(s)

    Code
      rec %>% step_count(description, rows, pattern = "(rock|stony)")
    Condition
      Error in `step_count()`:
      ! For this step, only a single selector can be used.

---

    Code
      prep(rec2, training = covers)
    Condition
      Error in `step_count()`:
      Caused by error in `prep()`:
      ! All columns selected for the step should be string, factor, or ordered.

# printing

    Code
      print(rec5)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 3
      
      -- Operations 
      * Regular expression counts using: description

---

    Code
      prep(rec5)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 3
      
      -- Training information 
      Training data contained 40 data points and no incomplete rows.
      
      -- Operations 
      * Regular expression counts using: description | Trained

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
      * Regular expression counts using: <none>

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
      * Regular expression counts using: <none> | Trained

