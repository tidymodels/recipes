# step_select() throws deprecating warning

    Code
      tmp <- step_select(recipe(~., mtcars), vs, mpg, disp)
    Condition
      Warning:
      `step_select()` was deprecated in recipes 1.3.0.
      i See `?step_select()` for recommended alternatives.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(rec, new_data = mtcars[, c(-2)])
    Condition
      Error in `step_select()`:
      ! The following required column is missing from `new_data`: cyl.

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
      * Variables selected: <none>

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
      * Variables selected: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 5
      
      -- Operations 
      * Variables selected: Species

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 5
      
      -- Training information 
      Training data contained 150 data points and no incomplete rows.
      
      -- Operations 
      * Variables selected: Species | Trained

