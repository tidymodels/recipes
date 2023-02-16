# bad selector(s)

    Code
      rec %>% step_regex(description, rows, pattern = "(rock|stony)")
    Condition
      Error in `step_regex()`:
      ! For this step, at most a single selector can be used.

---

    Code
      prep(rec4, training = covers)
    Condition
      Error in `step_regex()`:
      Caused by error in `prep()`:
      ! All columns selected for the step should be string, factor, or ordered.

# printing

    Code
      print(rec1)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 3
      
      -- Operations 
      * Regular expression dummy variable using: "(rock|stony)"

---

    Code
      prep(rec1)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 3
      
      -- Training information 
      Training data contained 40 data points and no incomplete rows.
      
      -- Operations 
      * Regular expression dummy variable using: "(rock|stony)" | Trained

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
      * Regular expression dummy variable using: "."

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
      * Regular expression dummy variable using: "." | Trained

