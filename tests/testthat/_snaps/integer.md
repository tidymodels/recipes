# bake method errors when needed non-standard role columns are missing

    Code
      bake(rec_trained, te_dat[, 2:3], all_predictors())
    Condition
      Error in `step_integer()`:
      ! The following required column is missing from `new_data`: x.

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
      * Integer encoding for: <none>

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
      * Integer encoding for: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 3
      
      -- Operations 
      * Integer encoding for: all_predictors()

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 3
      
      -- Training information 
      Training data contained 10 data points and 2 incomplete rows.
      
      -- Operations 
      * Integer encoding for: x, y, z | Trained

# bad args

    Code
      prep(step_integer(recipe(~ x + y + z, data = tr_dat), all_predictors(), strict = "yes"))
    Condition
      Error in `step_integer()`:
      Caused by error in `prep()`:
      ! `strict` must be `TRUE` or `FALSE`, not the string "yes".

---

    Code
      prep(step_integer(recipe(~ x + y + z, data = tr_dat), all_predictors(),
      zero_based = "sure!"))
    Condition
      Error in `step_integer()`:
      Caused by error in `prep()`:
      ! `zero_based` must be `TRUE` or `FALSE`, not the string "sure!".

