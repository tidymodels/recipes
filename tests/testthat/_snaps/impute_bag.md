# impute_with errors with nothing selected

    Code
      recipe(~., data = mtcars) %>% step_impute_bag(all_predictors(), impute_with = NULL) %>%
        prep()
    Condition
      Error in `step_impute_bag()`:
      ! `impute_with` must not be empty.

---

    Code
      tmp <- recipe(~., data = mtcars) %>% step_impute_bag(mpg, disp, vs) %>% prep()
    Condition
      Warning:
      All predictors are missing; cannot impute.
      Warning:
      All predictors are missing; cannot impute.
      Warning:
      All predictors are missing; cannot impute.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(imputed_trained, new_data = biomass[, c(-3, -9)])
    Condition
      Error in `step_impute_bag()`:
      ! The following required columns are missing from `new_data`: carbon and fac.

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

# printing

    Code
      print(rec)
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
      prep(rec)
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

# bad args

    Code
      recipe(~., data = mtcars) %>% step_impute_bag(all_predictors(), trees = -1) %>%
        prep()
    Condition
      Error in `step_impute_bag()`:
      Caused by error in `prep()`:
      ! `trees` must be a whole number larger than or equal to 1, not the number -1.

---

    Code
      recipe(~., data = mtcars) %>% step_impute_bag(all_predictors(), seed_val = 1:4) %>%
        prep()
    Condition
      Error in `step_impute_bag()`:
      Caused by error in `prep()`:
      ! `seed_val` must be a whole number, not an integer vector.

