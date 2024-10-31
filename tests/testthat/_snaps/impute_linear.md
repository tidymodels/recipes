# Fails when one of the variables to impute is non-numeric.

    Code
      recipe(tg_dat) %>% step_impute_linear(supp, impute_with = c("len")) %>% prep(
        tg_dat)
    Condition
      Error in `step_impute_linear()`:
      Caused by error in `prep()`:
      ! Variable `supp` chosen for linear regression imputation must be of type numeric. Not a string.

---

    Code
      recipe(tg_dat) %>% step_impute_linear(supp, dose, impute_with = c("len")) %>%
        prep(tg_dat)
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
      recipe(~., data = mtcars) %>% step_impute_linear(all_predictors(), impute_with = NULL) %>%
        prep()
    Condition
      Error in `step_impute_linear()`:
      ! `impute_with` must not be empty.

# warns if impute_with columns contains missing values

    Code
      tmp <- recipe(~., data = mtcars) %>% step_impute_linear(mpg, impute_with = imp_vars(
        disp)) %>% prep()
    Condition
      Warning:
      There were missing values in the predictor(s) used to impute; imputation did not occur.

# errors if there are no rows without missing values

    Code
      recipe(~., data = mtcars) %>% step_impute_linear(all_predictors()) %>% prep()
    Condition
      Error in `step_impute_linear()`:
      Caused by error in `prep()`:
      ! The data did not have any rows where the imputation values were all complete. Is is thus unable to fit the linear regression model.

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

