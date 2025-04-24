# input checking

    Code
      prep(step_relu(recipe(~., data = df), val1, shift = TRUE), df, verbose = FALSE)
    Condition
      Error in `step_relu()`:
      Caused by error in `prep()`:
      ! `shift` must be a number, not `TRUE`.

---

    Code
      prep(step_relu(recipe(~., data = df), val1, reverse = 3), df, verbose = FALSE)
    Condition
      Error in `step_relu()`:
      Caused by error in `prep()`:
      ! `reverse` must be `TRUE` or `FALSE`, not the number 3.

---

    Code
      prep(step_relu(recipe(~., data = df), val1, smooth = "cat"), df, verbose = FALSE)
    Condition
      Error in `step_relu()`:
      Caused by error in `prep()`:
      ! `smooth` must be `TRUE` or `FALSE`, not the string "cat".

---

    Code
      prep(step_relu(recipe(~., data = df), val2), df, verbose = FALSE)
    Condition
      Error in `step_relu()`:
      Caused by error in `prep()`:
      x All columns selected for the step should be double or integer.
      * 1 factor variable found: `val2`

# bake method errors when needed non-standard role columns are missing

    Code
      bake(rec, df[, 2, drop = FALSE])
    Condition
      Error in `step_relu()`:
      ! The following required column is missing from `new_data`: val1.

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
      * Adding relu transform for: <none>

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
      * Adding relu transform for: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 11
      
      -- Operations 
      * Adding relu transform for: disp

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
      * Adding relu transform for: disp | Trained

