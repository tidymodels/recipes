# spatial sign

    Code
      prep(step_spatialsign(rec, carbon, hydrogen, na_rm = 12))
    Condition
      Error in `step_spatialsign()`:
      Caused by error in `prep()`:
      ! `na_rm` must be `TRUE` or `FALSE`, not the number 12.

# centering with case weights

    Code
      rec
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:      1
      predictor:    9
      case_weights: 1
      
      -- Training information 
      Training data contained 32 data points and no incomplete rows.
      
      -- Operations 
      * Spatial sign on: disp, hp, drat, wt, qsec, vs, am, ... | Trained, weighted

---

    Code
      rec
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:      1
      predictor:    9
      case_weights: 1
      
      -- Training information 
      Training data contained 32 data points and no incomplete rows.
      
      -- Operations 
      * Spatial sign on: cyl, disp, hp, drat, qsec, ... | Trained, ignored weights

# bake method errors when needed non-standard role columns are missing

    Code
      bake(sp_sign_trained, new_data = biomass[, c(-3)])
    Condition
      Error in `step_spatialsign()`:
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
      * Spatial sign on: <none>

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
      * Spatial sign on: <none> | Trained

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
      * Centering for: carbon hydrogen
      * Scaling for: carbon hydrogen
      * Spatial sign on: carbon hydrogen

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
      * Centering for: carbon hydrogen | Trained
      * Scaling for: carbon hydrogen | Trained
      * Spatial sign on: carbon hydrogen | Trained

