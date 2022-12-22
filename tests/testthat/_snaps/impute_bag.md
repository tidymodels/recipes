# Deprecation warning

    Code
      recipe(~., data = mtcars) %>% step_bagimpute()
    Condition
      Error:
      ! `step_bagimpute()` was deprecated in recipes 0.1.16 and is now defunct.
      i Please use `step_impute_bag()` instead.

# printing

    Code
      print(imputed)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 6
      
      -- Operations 
      * Bagged tree imputation for: carbon

---

    Code
      prep(imputed)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 6
      
      -- Training information 
      Training data contained 536 data points and no incomplete rows.
      
      -- Operations 
      * Bagged tree imputation for: carbon | Trained

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
      * Bagged tree imputation for: <none>

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
      * Bagged tree imputation for: <none> | Trained

