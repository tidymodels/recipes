# wrong vars

    Code
      prep(rec2, training = examples, verbose = FALSE)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 2
      
      -- Training information 
      Training data contained 12 data points and no incomplete rows.
      
      -- Operations 
      * Unordered variables: X1 X2 | Trained

---

    Code
      prep(rec3, training = examples, verbose = FALSE)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 2
      
      -- Training information 
      Training data contained 12 data points and no incomplete rows.
      
      -- Operations 
      * Unordered variables: X1 | Trained

# bake method errors when needed non-standard role columns are missing

    Code
      bake(rec1_trained, new_data = examples[, 1, drop = FALSE])
    Condition
      Error in `step_unorder()`:
      ! The following required column is missing from `new_data`: X2.

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
      * Unordered variables: <none>

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
      * Unordered variables: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 2
      
      -- Operations 
      * Unordered variables: X2

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 2
      
      -- Training information 
      Training data contained 12 data points and no incomplete rows.
      
      -- Operations 
      * Unordered variables: X2 | Trained

