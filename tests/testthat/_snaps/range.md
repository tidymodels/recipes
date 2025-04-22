# warns when NaN is returned due to zero variance

    Code
      prep(rec)
    Condition
      Warning:
      Column `x` returned NaN. Consider using `step_zv()` to remove variables containing only a single value.
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 1
      
      -- Training information 
      Training data contained 10 data points and no incomplete rows.
      
      -- Operations 
      * Range scaling to [0,1] for: x | Trained

# warns when NaN is returned due to Inf or -Inf

    Code
      prep(rec)
    Condition
      Warning:
      Column `x` returned NaN. Consider avoiding `Inf` values before normalising.
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 1
      
      -- Training information 
      Training data contained 4 data points and no incomplete rows.
      
      -- Operations 
      * Range scaling to [0,1] for: x | Trained

---

    Code
      prep(rec)
    Condition
      Warning:
      Column `x` returned NaN. Consider avoiding `Inf` values before normalising.
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 1
      
      -- Training information 
      Training data contained 4 data points and no incomplete rows.
      
      -- Operations 
      * Range scaling to [0,1] for: x | Trained

# bake method errors when needed non-standard role columns are missing

    Code
      bake(standardized_trained, new_data = biomass_te[, 1:3])
    Condition
      Error in `step_range()`:
      ! The following required column is missing from `new_data`: hydrogen.

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
      * Range scaling to [0,1] for: <none>

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
      * Range scaling to [0,1] for: <none> | Trained

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
      * Range scaling to [0,1] for: disp wt

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
      * Range scaling to [0,1] for: disp wt | Trained

# bad args

    Code
      prep(step_range(recipe(mpg ~ ., data = mtcars), disp, wt, max = "max"))
    Condition
      Error in `step_range()`:
      Caused by error in `prep()`:
      ! `max` must be a number, not the string "max".

---

    Code
      prep(step_range(recipe(mpg ~ ., data = mtcars), disp, wt, min = "min"))
    Condition
      Error in `step_range()`:
      Caused by error in `prep()`:
      ! `min` must be a number, not the string "min".

---

    Code
      prep(step_range(recipe(mpg ~ ., data = mtcars), disp, wt, clipping = "never"))
    Condition
      Error in `step_range()`:
      Caused by error in `prep()`:
      ! `clipping` must be `TRUE` or `FALSE`, not the string "never".

