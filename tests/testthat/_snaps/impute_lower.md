# bad data

    Code
      rec %>% step_impute_lower(carbon, hydrogen, has_neg) %>% prep()
    Condition
      Error in `step_impute_lower()`:
      Caused by error in `prep()`:
      ! Some columns have negative values. Lower bound imputation is intended for data bounded at zero.

# Deprecation warning

    Code
      recipe(~., data = mtcars) %>% step_lowerimpute()
    Condition
      Error:
      ! `step_lowerimpute()` was deprecated in recipes 0.1.16 and is now defunct.
      i Please use `step_impute_lower()` instead.

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
      * Lower bound imputation for: <none>

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
      * Lower bound imputation for: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 3
      
      -- Operations 
      * Lower bound imputation for: carbon, hydrogen

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 3
      
      -- Training information 
      Training data contained 536 data points and no incomplete rows.
      
      -- Operations 
      * Lower bound imputation for: carbon, hydrogen | Trained

