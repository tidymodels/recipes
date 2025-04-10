# impute_with errors with nothing selected

    Code
      recipe(~., data = mtcars) %>% step_impute_bag(all_predictors(), impute_with = NULL) %>%
        prep()
    Condition
      Error in `step_impute_bag()`:
      Caused by error in `prep()`:
      ! `impute_with` must not be `NULL`.

# Warns when impute_with contains all NAs in a row

    Code
      tmp <- recipe(~., data = mtcars) %>% step_impute_bag(mpg, disp, vs,
        impute_with = c(am, gear)) %>% prep()
    Condition
      Warning:
      The `impute_with` variables for `mpg` only contains missing values for row: 2 and 3. Cannot impute for those rows.
      Warning:
      The `impute_with` variables for `disp` only contains missing values for row: 10. Cannot impute for those rows.

# Better error message for nzv fit error (#209)

    Code
      recipe(~., d) %>% step_impute_bag(let) %>% prep()
    Condition
      Error in `step_impute_bag()`:
      Caused by error in `prep()`:
      x The bagged tree model was not able to fit to `let`. It appears to be because it had near zero variance.
      i Please deselect it for this step.

# check_options() is used

    Code
      recipe(~mpg, data = mtcars) %>% step_impute_bag(mpg, options = TRUE) %>% prep()
    Condition
      Error in `step_impute_bag()`:
      Caused by error in `prep()`:
      ! `options` must be a list, not `TRUE`.

# recipes_argument_select() is used

    Code
      recipe(mpg ~ ., data = mtcars) %>% step_impute_bag(disp, impute_with = NULL) %>%
        prep()
    Condition
      Error in `step_impute_bag()`:
      Caused by error in `prep()`:
      ! `impute_with` must not be `NULL`.

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

