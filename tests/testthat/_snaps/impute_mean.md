# non-numeric

    Code
      prep(impute_rec, training = credit_tr, verbose = FALSE)
    Condition
      Error in `step_impute_mean()`:
      Caused by error in `prep()`:
      x All columns selected for the step should be double or integer.
      * 1 factor variable found: `Job`

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
      * Mean imputation for: Age, Assets, Income | Trained, weighted

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
      * Mean imputation for: Age, Assets, Income | Trained, ignored weights

# bake method errors when needed non-standard role columns are missing

    Code
      bake(imputed, new_data = credit_te[, c(-5)])
    Condition
      Error in `step_impute_mean()`:
      ! The following required column is missing from `new_data`: Age.

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
      * Mean imputation for: <none>

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
      * Mean imputation for: <none> | Trained

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
      * Mean imputation for: Age, Assets, Income

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
      * Mean imputation for: Age, Assets, Income | Trained

# bad args

    Code
      prep(step_impute_mean(recipe(~., data = mtcars), all_predictors(), trim = 0.6))
    Condition
      Error in `step_impute_mean()`:
      Caused by error in `prep()`:
      ! `trim` must be a number between 0 and 0.5, not the number 0.6.

