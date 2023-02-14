# printing

    Code
      print(int_rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 6
      
      -- Operations 
      * Interactions with: x1:x2

---

    Code
      prep(int_rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 6
      
      -- Training information 
      Training data contained 10 data points and no incomplete rows.
      
      -- Operations 
      * Interactions with: x1:x2 | Trained

# bake method errors when needed non-standard role columns are missing

    Code
      bake(int_rec_trained, dat_tr[, 4:6])
    Condition
      Error in `step_interact()`:
      ! The following required columns are missing from `new_data` in step '': z and x1.

