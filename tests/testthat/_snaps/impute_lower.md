# bad data

    Code
      prep(step_impute_lower(rec, carbon, hydrogen, has_neg))
    Condition
      Error in `step_impute_lower()`:
      Caused by error in `prep()`:
      x The following columns negative values: has_neg.
      i Lower bound imputation is intended for data bounded at zero.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(imputed_trained, new_data = biomass[, 4:7])
    Condition
      Error in `step_impute_lower()`:
      ! The following required column is missing from `new_data`: carbon.

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
      * Lower bound imputation for: <none>

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
      * Lower bound imputation for: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 3
      
      -- Operations 
      * Lower bound imputation for: carbon hydrogen

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 3
      
      -- Training information 
      Training data contained 536 data points and no incomplete rows.
      
      -- Operations 
      * Lower bound imputation for: carbon hydrogen | Trained

