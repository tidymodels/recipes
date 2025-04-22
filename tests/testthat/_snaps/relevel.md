# bad args

    Code
      prep(step_relevel(rec, sqft, ref_level = 23))
    Condition
      Error in `step_relevel()`:
      Caused by error in `prep()`:
      x All columns selected for the step should be string or factor.
      * 1 integer variable found: `sqft`

---

    Code
      prep(step_relevel(rec, city, ref_level = "missing_level"))
    Condition
      Error in `step_relevel()`:
      Caused by error in `prep()`:
      ! The following column doesn't include required reference level "missing_level": `city`.

---

    Code
      prep(step_relevel(rec, city, ref_level = character(0)))
    Condition
      Error in `step_relevel()`:
      Caused by error in `prep()`:
      ! `ref_level` must be a single string, not an empty character vector.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(rec_1, sacr_te[, c(1, 3:ncol(sacr_te))])
    Condition
      Error in `step_relevel()`:
      ! The following required column is missing from `new_data`: zip.

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
      * Re-order factor level to ref_level for: <none>

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
      * Re-order factor level to ref_level for: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 9
      
      -- Operations 
      * Re-order factor level to ref_level for: zip

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 9
      
      -- Training information 
      Training data contained 800 data points and no incomplete rows.
      
      -- Operations 
      * Re-order factor level to ref_level for: zip | Trained

