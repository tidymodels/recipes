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
      * Unordered variables: X1, X2 | Trained

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

# printing

    Code
      print(rec4)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 2
      
      -- Operations 
      * Unordered variables: X2

---

    Code
      prep(rec4)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 2
      
      -- Training information 
      Training data contained 12 data points and no incomplete rows.
      
      -- Operations 
      * Unordered variables: X2 | Trained

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

