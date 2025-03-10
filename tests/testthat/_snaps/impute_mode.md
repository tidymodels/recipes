# non-nominal

    Code
      prep(impute_rec, training = credit_tr, verbose = FALSE)
    Condition
      Error in `step_impute_mode()`:
      Caused by error in `prep()`:
      ! The data should be character or factor to compute the mode. Not an integer vector.

# can bake recipes with no ptype

    Code
      imputed_te <- bake(imputed, credit_te)
    Condition
      Warning:
      ! `ptype` was added to `step_impute_mode()` after this recipe was created.
      i Regenerate your recipe to avoid this warning.

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

# bake method errors when needed non-standard role columns are missing

    Code
      bake(imputed, new_data = credit_te[, c(-6)])
    Condition
      Error in `step_impute_mode()`:
      ! The following required column is missing from `new_data`: Marital.

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

# printing

    Code
      print(rec)
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
      prep(rec)
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

