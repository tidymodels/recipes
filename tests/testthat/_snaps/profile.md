# bad values

    Code
      prep(step_profile(sacr_rec, all_predictors(), profile = sqft), data = Sacramento)
    Condition
      Error in `step_profile()`:
      Caused by error in `prep()`:
      ! The profiled variable cannot be in the list of variables to be fixed. `sqft` was in both.

---

    Code
      prep(step_profile(sacr_rec, sqft, beds, price, profile = c(zip, beds)), data = Sacramento)
    Condition
      Error in `step_profile()`:
      Caused by error in `prep()`:
      ! only 1 selection is allowed in `profile`, not 2.

---

    Code
      prep(step_profile(sacr_rec, city, profile = sqft, pct = -1), data = Sacramento)
    Condition
      Error in `step_profile()`:
      ! `pct` must be a number between 0 and 1, not the number -1.

---

    Code
      prep(step_profile(sacr_rec, city, profile = sqft, grid = 1:3), data = Sacramento)
    Condition
      Error in `step_profile()`:
      x `grid` should have 2 elements, not 3.
      i See ?step_profile (`?recipes::step_profile()`) for information.

---

    Code
      prep(step_profile(sacr_rec, city, profile = sqft, grid = list(pctl = 1, len = 2)),
      data = Sacramento)
    Condition
      Error in `step_profile()`:
      ! `grid$pctl` must be `TRUE` or `FALSE`, not the number 1.

---

    Code
      fixed(rep(c(TRUE, FALSE), each = 5))
    Condition
      Error in `fixed()`:
      ! No method for determining a value to fix for objects of class: <logical>.

# error on wrong grid names

    Code
      step_profile(recipe(~., data = mtcars), grid = list(pctl = TRUE, not_len = 100))
    Condition
      Error in `step_profile()`:
      x `grid` should have two named elements len and pctl, not "not_len" and "pctl".
      i See ?step_profile (`?recipes::step_profile()`) for information.

# recipes_argument_select() is used

    Code
      prep(step_profile(recipe(mpg ~ ., data = mtcars), disp, profile = NULL))
    Condition
      Error in `step_profile()`:
      Caused by error in `prep()`:
      ! `profile` must not be `NULL`.

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
      * Profiling data set for: mpg

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
      * Profiling data set for: mpg | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 10
      
      -- Operations 
      * Profiling data set for: sqft

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 10
      
      -- Training information 
      Training data contained 20 data points and no incomplete rows.
      
      -- Operations 
      * Profiling data set for: sqft | Trained

