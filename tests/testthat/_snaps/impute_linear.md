# Fails when one of the variables to impute is non-numeric.

    Code
      prep(step_impute_linear(recipe(tg_dat), supp, impute_with = len), tg_dat)
    Condition
      Error in `step_impute_linear()`:
      Caused by error in `prep()`:
      ! Variable `supp` chosen for linear regression imputation must be of type numeric. Not a string.

---

    Code
      prep(step_impute_linear(recipe(tg_dat), supp, dose, impute_with = len), tg_dat)
    Condition
      Error in `step_impute_linear()`:
      Caused by error in `prep()`:
      ! Variable `supp` chosen for linear regression imputation must be of type numeric. Not a string.

# case weights

    Code
      rec_prepped
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      case_weights:    1
      undeclared role: 2
      
      -- Training information 
      Training data contained 2930 data points and 556 incomplete rows.
      
      -- Operations 
      * Linear regression imputation for: Lot_Frontage | Trained, weighted

---

    Code
      rec_prepped
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      case_weights:    1
      undeclared role: 2
      
      -- Training information 
      Training data contained 2930 data points and 556 incomplete rows.
      
      -- Operations 
      * Linear regression imputation for: Lot_Frontage | Trained, ignored weights

# impute_with errors with nothing selected

    Code
      prep(step_impute_linear(recipe(~., data = mtcars), all_predictors(),
      impute_with = NULL))
    Condition
      Error in `step_impute_linear()`:
      Caused by error in `prep()`:
      ! `impute_with` must not be `NULL`.

# warns if impute_with columns contains missing values

    Code
      tmp <- prep(step_impute_linear(recipe(~., data = mtcars), mpg, impute_with = disp))
    Condition
      Warning:
      There were missing values in the predictor(s) used to impute; imputation did not occur.

# errors if there are no rows without missing values

    Code
      prep(step_impute_linear(recipe(~., data = mtcars), all_predictors()))
    Condition
      Error in `step_impute_linear()`:
      Caused by error in `prep()`:
      ! The data did not have any rows where the imputation values were all complete. Is is thus unable to fit the linear regression model.

# recipes_argument_select() is used

    Code
      prep(step_impute_linear(recipe(mpg ~ ., data = mtcars), disp, impute_with = NULL))
    Condition
      Error in `step_impute_linear()`:
      Caused by error in `prep()`:
      ! `impute_with` must not be `NULL`.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(rec, new_data = ames_dat[, 2:3])
    Condition
      Error in `step_impute_linear()`:
      ! The following required column is missing from `new_data`: Lot_Frontage.

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
      * Linear regression imputation for: <none>

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
      * Linear regression imputation for: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      undeclared role: 3
      
      -- Operations 
      * Linear regression imputation for: Lot_Frontage

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      undeclared role: 3
      
      -- Training information 
      Training data contained 2930 data points and 556 incomplete rows.
      
      -- Operations 
      * Linear regression imputation for: Lot_Frontage | Trained

