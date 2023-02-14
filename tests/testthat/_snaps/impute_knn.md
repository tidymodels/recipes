# Deprecation warning

    Code
      recipe(~., data = mtcars) %>% step_knnimpute()
    Condition
      Error:
      ! `step_knnimpute()` was deprecated in recipes 0.1.16 and is now defunct.
      i Please use `step_impute_knn()` instead.

# printing

    Code
      print(discr_rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 5
      
      -- Operations 
      * K-nearest neighbor imputation for: carbon, nitrogen

---

    Code
      prep(discr_rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 5
      
      -- Training information 
      Training data contained 536 data points and no incomplete rows.
      
      -- Operations 
      * K-nearest neighbor imputation for: carbon, nitrogen | Trained

# options

    Code
      new_nn <- gower_topn(x = dat_2, y = dat_1, n = 2, eps = 2)$index
    Condition
      Warning in `gower_work()`:
      skipping variable with zero or non-finite range.

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
      * K-nearest neighbor imputation for: <none>

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
      * K-nearest neighbor imputation for: <none> | Trained

