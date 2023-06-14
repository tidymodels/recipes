# non-numeric

    Code
      prep(impute_rec, training = credit_tr, verbose = FALSE)
    Condition
      Error in `step_impute_median()`:
      Caused by error in `prep()`:
      ! All columns selected for the step should be double, or integer.

# Deprecation warning

    Code
      recipe(~., data = mtcars) %>% step_medianimpute()
    Condition
      Error:
      ! `step_medianimpute()` was deprecated in recipes 0.1.16 and is now defunct.
      i Please use `step_impute_median()` instead.

# case weights

    Code
      impute_rec
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:       1
      predictor:    12
      case_weights:  1
      
      -- Training information 
      Training data contained 2000 data points and 186 incomplete rows.
      
      -- Operations 
      * Median imputation for: Age, Assets, Income | Trained, weighted

---

    Code
      impute_rec
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:       1
      predictor:    12
      case_weights:  1
      
      -- Training information 
      Training data contained 2000 data points and 186 incomplete rows.
      
      -- Operations 
      * Median imputation for: Age, Assets, Income | Trained, ignored weights

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
      * Median imputation for: <none>

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
      * Median imputation for: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 13
      
      -- Operations 
      * Median imputation for: Age, Assets, Income

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 13
      
      -- Training information 
      Training data contained 2000 data points and 186 incomplete rows.
      
      -- Operations 
      * Median imputation for: Age, Assets, Income | Trained

