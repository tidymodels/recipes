# 'other' already in use

    Code
      prep(others, training = sacr_tr_chr)
    Condition
      Error in `step_other()`:
      Caused by error in `prep()`:
      ! The level other is already a factor level that will be retained. Please choose a different value.

# if the threshold argument is greather than one then it should be an integer(ish)

    Code
      prep(step_other(rec, city, zip, threshold = 3.14))
    Condition
      Error in `step_other()`:
      Caused by error in `prep()`:
      ! `threshold` must be a whole number, not the number 3.14.

# bad values of threshold are treated correctly

    Code
      prep(step_other(rec, city, zip, threshold = letters))
    Condition
      Error in `step_other()`:
      Caused by error in `prep()`:
      ! `threshold` should be a single numeric value a character vector

# othering with case weights

    Code
      others
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor:    1
      case_weights: 1
      
      -- Training information 
      Training data contained 732 data points and no incomplete rows.
      
      -- Operations 
      * Collapsing factor levels for: city | Trained, weighted

---

    Code
      others
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor:    1
      case_weights: 1
      
      -- Training information 
      Training data contained 732 data points and no incomplete rows.
      
      -- Operations 
      * Collapsing factor levels for: city | Trained, ignored weights

# bake method errors when needed non-standard role columns are missing

    Code
      bake(others, new_data = sacr_te[, 3:9])
    Condition
      Error in `step_other()`:
      ! The following required columns are missing from `new_data`: city and zip.

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
      * Collapsing factor levels for: <none>

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
      * Collapsing factor levels for: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 2
      
      -- Operations 
      * Collapsing factor levels for: city zip

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 2
      
      -- Training information 
      Training data contained 732 data points and no incomplete rows.
      
      -- Operations 
      * Collapsing factor levels for: city zip | Trained

