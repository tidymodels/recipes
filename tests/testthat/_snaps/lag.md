# default lag works on a single feature

    Code
      prepped_rec <- prep(step_lag(recipe(~., data = df), x, lag = 0.5), df)
    Condition
      Error in `step_lag()`:
      Caused by error in `prep()`:
      ! `lag` argument must be integer-valued, not a function.

---

    Code
      prep(step_lag(recipe(~., data = df), x, prefix = 2))
    Condition
      Error in `step_lag()`:
      Caused by error in `prep()`:
      ! `prefix` must be a single string, not the number 2.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(rec, new_data = df[, 1, drop = FALSE])
    Condition
      Error in `step_lag()`:
      ! The following required column is missing from `new_data`: t.

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
      * Lagging: <none>

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
      * Lagging: <none> | Trained

# keep_original_cols - can prep recipes with it missing

    Code
      rec <- prep(rec)
    Condition
      Warning:
      `keep_original_cols` was added to `step_lag()` after this recipe was created.
      i Regenerate your recipe to avoid this warning.

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 11
      
      -- Operations 
      * Lagging: disp

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 11
      
      -- Training information 
      Training data contained 32 data points and no incomplete rows.
      
      -- Operations 
      * Lagging: disp | Trained

