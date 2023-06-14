# bad options

    Code
      prep(rec3, training = covers)
    Condition
      Error in `step_bin2factor()`:
      Caused by error in `prep()`:
      ! All columns selected for the step should be double, integer, or logical.

---

    Code
      rec %>% step_bin2factor(rocks, levels = letters[1:5])
    Condition
      Error in `step_bin2factor()`:
      ! `levels` should be a two element character string

---

    Code
      rec %>% step_bin2factor(rocks, levels = 1:2)
    Condition
      Error in `step_bin2factor()`:
      ! `levels` should be a two element character string

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
      * Dummy variable to factor conversion for: <none>

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
      * Dummy variable to factor conversion for: <none> | Trained

# printing

    Code
      print(rec2)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 1
      
      -- Operations 
      * Regular expression dummy variable using: "(rock|stony)"
      * Regular expression dummy variable using: "(rock|stony)"
      * Dummy variable to factor conversion for: rocks

---

    Code
      prep(rec2)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 1
      
      -- Training information 
      Training data contained 40 data points and no incomplete rows.
      
      -- Operations 
      * Regular expression dummy variable using: "(rock|stony)" | Trained
      * Regular expression dummy variable using: "(rock|stony)" | Trained
      * Dummy variable to factor conversion for: rocks | Trained

