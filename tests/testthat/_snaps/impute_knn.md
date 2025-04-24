# options

    Code
      new_nn <- gower_topn(x = dat_2, y = dat_1, n = 2, eps = 2)$index
    Condition
      Warning in `gower_work()`:
      skipping variable with zero or non-finite range.

# impute_with errors with nothing selected

    Code
      prep(step_impute_knn(recipe(~., data = mtcars), all_predictors(), impute_with = NULL))
    Condition
      Error in `step_impute_knn()`:
      Caused by error in `prep()`:
      ! `impute_with` must not be `NULL`.

# Warns when impute_with contains all NAs in a row

    Code
      tmp <- prep(step_impute_knn(recipe(~., data = mtcars), mpg, disp, vs,
      impute_with = c(am, gear)))
    Condition
      Warning:
      The `impute_with` variables for `mpg` only contains missing values for row: 2 and 3. Cannot impute for those rows.
      Warning:
      The `impute_with` variables for `disp` only contains missing values for row: 10. Cannot impute for those rows.

# error on wrong options argument

    Code
      prep(step_impute_knn(recipe(~., data = mtcars), all_predictors(), options = list(
        wrong = "wrong")))
    Condition
      Error in `step_impute_knn()`:
      Caused by error in `prep()`:
      ! `options` must only contain elements nthread and eps, the following are not allowed: wrong.

---

    Code
      prep(step_impute_knn(recipe(~., data = mtcars), all_predictors(), options = c(
        wrong = "wrong")))
    Condition
      Error in `step_impute_knn()`:
      Caused by error in `prep()`:
      ! `options` must be a list, not a string.

# check_options() is used

    Code
      prep(step_impute_knn(recipe(~mpg, data = mtcars), mpg, options = TRUE))
    Condition
      Error in `step_impute_knn()`:
      Caused by error in `prep()`:
      ! `options` must be a list, not `TRUE`.

# recipes_argument_select() is used

    Code
      prep(step_impute_knn(recipe(mpg ~ ., data = mtcars), disp, impute_with = NULL))
    Condition
      Error in `step_impute_knn()`:
      Caused by error in `prep()`:
      ! `impute_with` must not be `NULL`.

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
      * K-nearest neighbor imputation for: carbon nitrogen

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
      * K-nearest neighbor imputation for: carbon nitrogen | Trained

# bad args

    Code
      prep(step_impute_knn(recipe(~., data = mtcars), all_predictors(), neighbors = 0L))
    Condition
      Error in `step_impute_knn()`:
      Caused by error in `prep()`:
      ! `neighbors` must be a whole number larger than or equal to 1, not the number 0.

