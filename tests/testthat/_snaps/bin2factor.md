# works with logicals

    Code
      step_bin2factor(recipe(~., data = mtcars), all_logical_predictors(), ref_first = 1)
    Condition
      Error in `step_bin2factor()`:
      ! `ref_first` must be `TRUE` or `FALSE`, not the number 1.

# bad options

    Code
      prep(rec3, training = covers)
    Condition
      Error in `step_bin2factor()`:
      Caused by error in `prep()`:
      x All columns selected for the step should be double, integer, or logical.
      * 1 factor variable found: `description`

---

    Code
      step_bin2factor(rec, rocks, levels = letters[1:5])
    Condition
      Error in `step_bin2factor()`:
      x `levels` should be a 2-element character string.
      i 5 elements were supplied; two were expected.

---

    Code
      step_bin2factor(rec, rocks, levels = 1:2)
    Condition
      Error in `step_bin2factor()`:
      x `levels` should be a 2-element character string.
      i It was an integer vector.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(rec, mtcars)
    Condition
      Error in `step_bin2factor()`:
      ! The following required column is missing from `new_data`: bin.

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
      * Dummy variable to factor conversion for: <none>

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
      * Dummy variable to factor conversion for: <none> | Trained

# printing

    Code
      print(rec2)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 1
      
      -- Operations 
      * Regular expression dummy variable using: "(rock|stony)"
      * Regular expression dummy variable using: "(rock|stony)"
      * Dummy variable to factor conversion for: rocks

---

    Code
      prep(rec2)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 1
      
      -- Training information 
      Training data contained 40 data points and no incomplete rows.
      
      -- Operations 
      * Regular expression dummy variable using: "(rock|stony)" | Trained
      * Regular expression dummy variable using: "(rock|stony)" | Trained
      * Dummy variable to factor conversion for: rocks | Trained

