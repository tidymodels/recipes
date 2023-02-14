# Fails when one of the variables to impute is non-numeric.

    Code
      recipe(tg_dat) %>% step_impute_linear(supp, impute_with = c("len")) %>% prep(
        tg_dat)
    Condition
      Error in `step_impute_linear()`:
      Caused by error in `prep()`:
      ! Variable 'supp' chosen for linear regression imputation must be of type numeric.

---

    Code
      recipe(tg_dat) %>% step_impute_linear(supp, dose, impute_with = c("len")) %>%
        prep(tg_dat)
    Condition
      Error in `step_impute_linear()`:
      Caused by error in `prep()`:
      ! Variable 'supp' chosen for linear regression imputation must be of type numeric.

# Printing

    Code
      print(imputed)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      undeclared role: 3
      
      -- Operations 
      * Linear regression imputation for: Lot_Frontage

---

    Code
      prep(imputed)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      undeclared role: 3
      
      -- Training information 
      Training data contained 2930 data points and 556 incomplete rows.
      
      -- Operations 
      * Linear regression imputation for: Lot_Frontage | Trained

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

