# printing

    Code
      print(rec3)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 4
      
      -- Operations 
      * Shuffled: everything()

---

    Code
      prep(rec3)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 4
      
      -- Training information 
      Training data contained 50 data points and no incomplete rows.
      
      -- Operations 
      * Shuffled: x1, x2, x3, x4, y | Trained

# bake a single row

    Code
      dat4 <- bake(rec4, dat[1, ], everything())
    Condition
      Warning:
      `new_data` contains a single row; unable to shuffle

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
      * Shuffled: <none>

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
      * Shuffled: <none> | Trained

