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
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 13
      
      -- Operations 
      * Mode imputation for: Status, Home, Marital

---

    Code
      prep(impute_rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 13
      
      -- Training information 
      Training data contained 2000 data points and 186 incomplete rows.
      
      -- Operations 
      * Mode imputation for: Status, Home, Marital | Trained

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
      * Mode imputation for: <none>

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
      * Mode imputation for: <none> | Trained

# case weights

    Code
      imputed
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor:    1
      case_weights: 1
      
      -- Training information 
      Training data contained 150 data points and 10 incomplete rows.
      
      -- Operations 
      * Mode imputation for: x1 | Trained, weighted

---

    Code
      imputed
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor:    1
      case_weights: 1
      
      -- Training information 
      Training data contained 150 data points and 10 incomplete rows.
      
      -- Operations 
      * Mode imputation for: x1 | Trained, ignored weights

