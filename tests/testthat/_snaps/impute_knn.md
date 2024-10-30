# options

    Code
      new_nn <- gower_topn(x = dat_2, y = dat_1, n = 2, eps = 2)$index
    Condition
      Warning in `gower_work()`:
      skipping variable with zero or non-finite range.

# impute_with errors with nothing selected

    Code
      recipe(~., data = mtcars) %>% step_impute_knn(all_predictors(), impute_with = NULL) %>%
        prep()
    Condition
      Error in `step_impute_knn()`:
      ! `impute_with` must not be empty.

# warn if all values of predictor are missing

    Code
      tmp <- recipe(~., data = mtcars) %>% step_impute_knn(mpg, disp, vs) %>% prep()
    Condition
      Warning:
      All predictors are missing; cannot impute.
      Warning:
      All predictors are missing; cannot impute.
      Warning:
      All predictors are missing; cannot impute.

# error on wrong options argument

    Code
      recipe(~., data = mtcars) %>% step_impute_knn(all_predictors(), options = list(
        wrong = "wrong")) %>% prep()
    Condition
      Error in `step_impute_knn()`:
      ! Valid values for `options` are "eps" and "nthread".

---

    Code
      recipe(~., data = mtcars) %>% step_impute_knn(all_predictors(), options = c(
        wrong = "wrong")) %>% prep()
    Condition
      Error in `step_impute_knn()`:
      ! `options` should be a named list.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(imputed_trained, new_data = biomass[, c(-4)])
    Condition
      Error in `step_impute_knn()`:
      ! The following required column is missing from `new_data`: hydrogen.

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

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 5
      
      -- Operations 
      * K-nearest neighbor imputation for: carbon and nitrogen

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 5
      
      -- Training information 
      Training data contained 536 data points and no incomplete rows.
      
      -- Operations 
      * K-nearest neighbor imputation for: carbon and nitrogen | Trained

# bad args

    Code
      recipe(~., data = mtcars) %>% step_impute_knn(all_predictors(), neighbors = 0L) %>%
        prep()
    Condition
      Error in `step_impute_knn()`:
      Caused by error in `prep()`:
      ! `neighbors` must be a whole number larger than or equal to 1, not the number 0.

