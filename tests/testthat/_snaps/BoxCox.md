# simple Box Cox

    Code
      rec_trained <- prep(rec, training = ex_dat, verbose = FALSE)
    Condition
      Warning:
      Non-positive values in selected variable.
      Warning:
      Fewer than `num_unique` values in selected variable.
      Warning:
      No Box-Cox transformation could be estimated for: `x2` and `x3`.

# warnings

    Code
      prep(step_BoxCox(recipe(~., data = exp_dat), x1))
    Condition
      Warning:
      Non-positive values in selected variable.
      Warning:
      No Box-Cox transformation could be estimated for: `x1`.
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 4
      
      -- Training information 
      Training data contained 20 data points and no incomplete rows.
      
      -- Operations 
      * Box-Cox transformation on: <none> | Trained

---

    Code
      bake(prep(step_BoxCox(recipe(~ mpg + disp, data = mtcars), mpg, disp)),
      new_data = tibble(mpg = -1, disp = -1))
    Condition
      Warning:
      Applying Box-Cox transformation to non-positive data in column mpg.
      Warning:
      Applying Box-Cox transformation to non-positive data in column disp.
    Output
      # A tibble: 1 x 2
          mpg  disp
        <dbl> <dbl>
      1   NaN   NaN

# bake method errors when needed non-standard role columns are missing

    Code
      bake(rec_trained, new_data = ex_dat[, 1:2])
    Condition
      Error in `step_BoxCox()`:
      ! The following required column is missing from `new_data`: x4.

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
      * Box-Cox transformation on: <none>

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
      * Box-Cox transformation on: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 4
      
      -- Operations 
      * Box-Cox transformation on: x1, x2, x3, x4

---

    Code
      prep(rec)
    Condition
      Warning:
      Non-positive values in selected variable.
      Warning:
      Fewer than `num_unique` values in selected variable.
      Warning:
      No Box-Cox transformation could be estimated for: `x2` and `x3`.
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 4
      
      -- Training information 
      Training data contained 20 data points and no incomplete rows.
      
      -- Operations 
      * Box-Cox transformation on: x1 x4 | Trained

