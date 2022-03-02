# Fails when one of the variables to impute is non-numeric.

    Code
      recipe(tg_dat) %>% step_impute_linear(supp, impute_with = c("len")) %>% prep(
        tg_dat)
    Condition
      Error in `FUN()`:
      ! Variable 'supp' chosen for linear regression imputation must be of type numeric.

---

    Code
      recipe(tg_dat) %>% step_impute_linear(supp, dose, impute_with = c("len")) %>%
        prep(tg_dat)
    Condition
      Error in `FUN()`:
      ! Variable 'supp' chosen for linear regression imputation must be of type numeric.

# Prints.

    Code
      print(imputed)
    Output
      Recipe
      
      Inputs:
      
        3 variables (no declared roles)
      
      Operations:
      
      Linear regression imputation for Lot_Frontage

---

    Code
      prep(imputed, training = ames_dat, verbose = TRUE)
    Output
      oper 1 step impute linear [training] 
      The retained training set is ~ 0.06 Mb  in memory.
      
      Recipe
      
      Inputs:
      
        3 variables (no declared roles)
      
      Training data contained 2930 data points and 556 incomplete rows. 
      
      Operations:
      
      Linear regression imputation for Lot_Frontage [trained]

# empty printing

    Code
      rec
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor         10
      
      Operations:
      
      Linear regression imputation for <none>

---

    Code
      rec
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor         10
      
      Training data contained 32 data points and no missing data.
      
      Operations:
      
      Linear regression imputation for <none> [trained]

