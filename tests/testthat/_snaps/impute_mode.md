# non-nominal

    Code
      prep(impute_rec, training = credit_tr, verbose = FALSE)
    Condition
      Error in `step_impute_mode()`:
      Caused by error in `prep()`:
      ! The data should be character or factor to compute the mode.

# can bake recipes with no ptype

    Code
      imputed_te <- bake(imputed, credit_te)
    Condition
      Warning:
      'ptype' was added to `step_impute_mode()` after this recipe was created.
      Regenerate your recipe to avoid this warning.

# Deprecation warning

    Code
      recipe(~., data = mtcars) %>% step_modeimpute()
    Condition
      Error:
      ! `step_modeimpute()` was deprecated in recipes 0.1.16 and is now defunct.
      i Please use `step_impute_mode()` instead.

# printing

    Code
      print(impute_rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor         13
      
      Operations:
      
      Mode imputation for Status, Home, Marital

---

    Code
      prep(impute_rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor         13
      
      Training data contained 2000 data points and 186 incomplete rows. 
      
      Operations:
      
      Mode imputation for Status, Home, Marital [trained]

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
      
      Mode imputation for <none>

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
      
      Mode imputation for <none> [trained]

# case weights

    Code
      imputed
    Output
      Recipe
      
      Inputs:
      
               role #variables
       case_weights          1
          predictor          1
      
      Training data contained 150 data points and 10 incomplete rows. 
      
      Operations:
      
      Mode imputation for x1 [weighted, trained]

---

    Code
      imputed
    Output
      Recipe
      
      Inputs:
      
               role #variables
       case_weights          1
          predictor          1
      
      Training data contained 150 data points and 10 incomplete rows. 
      
      Operations:
      
      Mode imputation for x1 [ignored weights, trained]

