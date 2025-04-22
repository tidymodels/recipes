# basic functionality

    Code
      te_1 <- bake(rec_1, sacr_te)
    Condition
      Warning:
      ! There are new levels in `city`: "WEST_SACRAMENTO".
      i Consider using step_novel() (`?recipes::step_novel()`) before `step_unknown()` to handle unseen values.
      * New levels will be coerced to `NA` by `step_unknown()`.
      Warning:
      ! There are new levels in `zip`: "z95691".
      i Consider using step_novel() (`?recipes::step_novel()`) before `step_unknown()` to handle unseen values.
      * New levels will be coerced to `NA` by `step_unknown()`.

# bad args

    Code
      prep(step_unknown(recipe(~., data = sacr_tr), sqft))
    Condition
      Error in `step_unknown()`:
      Caused by error in `prep()`:
      x All columns selected for the step should be string, factor, or ordered.
      * 1 integer variable found: `sqft`

---

    Code
      prep(step_unknown(recipe(~., data = sacr_tr), city, new_level = "FAIR_OAKS"))
    Condition
      Error in `step_unknown()`:
      Caused by error in `prep()`:
      ! Columns already contain the level "FAIR_OAKS": city.

---

    Code
      prep(step_unknown(recipe(~., data = sacr_tr), city, new_level = 2))
    Condition
      Error in `step_unknown()`:
      Caused by error in `prep()`:
      ! `new_level` must be a single string, not the number 2.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(rec_1, sacr_te[3:ncol(sacr_te)])
    Condition
      Error in `step_unknown()`:
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
      * Unknown factor level assignment for: <none>

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
      * Unknown factor level assignment for: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 9
      
      -- Operations 
      * Unknown factor level assignment for: city zip

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
      * Unknown factor level assignment for: city zip | Trained

