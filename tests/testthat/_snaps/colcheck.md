# check_col works in the bake stage

    Code
      bake(prep(check_cols(rp1, all_predictors())), mtcars[-1])
    Condition
      Error in `bake()`:
      x The following required columns are missing from `new_data`: `mpg`.
      i These columns have one of the following roles, which are required at `bake()` time: `predictor`.

---

    Code
      bake(prep(check_cols(rp2, cyl, mpg, drat)), mtcars[, c(2, 5)])
    Condition
      Error in `bake()`:
      x The following required columns are missing from `new_data`: `mpg`.
      i These columns have one of the following roles, which are required at `bake()` time: `predictor`.

# non-standard roles during bake/predict

    Code
      predict(role_wts_fit, select(head(Chicago), -date))
    Condition
      Error in `hardhat::forge()`:
      ! The required column "date" is missing.

---

    Code
      predict(rm_fit, select(Chicago, -date))
    Condition
      Error in `hardhat::forge()`:
      ! The required column "date" is missing.

---

    Code
      predict(rm_fit, select(Chicago, -date))
    Condition
      Error in `hardhat::forge()`:
      ! The required column "date" is missing.

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
      * Check if the following columns are present:: <none>

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
      * Check if the following columns are present:: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 10
      
      -- Operations 
      * Check if the following columns are present:: all_predictors()

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 10
      
      -- Training information 
      Training data contained 32 data points and no incomplete rows.
      
      -- Operations 
      * Check if the following columns are present:: cyl, disp, hp, ... | Trained

