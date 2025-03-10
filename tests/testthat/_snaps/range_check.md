# core function - correct input

    Code
      range_check_func(as.character(x), -10, 110)
    Condition
      Error in `range_check_func()`:
      ! `x` must be a numeric vector, not a character vector.

---

    Code
      range_check_func(x, -10, 110, "a")
    Condition
      Error in `range_check_func()`:
      ! `slack_prop` must be a numeric vector, not a string.

---

    Code
      range_check_func(x, -10, 110, c(0.05, 0.08, 0.05))
    Condition
      Error in `range_check_func()`:
      ! `slack_prop` should be of length 1 or 2, not 3.

# core function - workings

    Code
      range_check_func(x, 0, 100)
    Condition
      Error in `range_check_func()`:
      i Smallest value of `x` is -10, crossing the lower bound -5.
      i Largest value of `x` is 110, crossing the upper bound 105.

---

    Code
      range_check_func(x, 0, 110)
    Condition
      Error in `range_check_func()`:
      i Smallest value of `x` is -10, crossing the lower bound -5.5.

---

    Code
      range_check_func(x, -5, 100)
    Condition
      Error in `range_check_func()`:
      i Largest value of `x` is 110, crossing the upper bound 105.25.

---

    Code
      range_check_func(x, 0, 100, slack_prop = c(0.05, 0.1))
    Condition
      Error in `range_check_func()`:
      i Smallest value of `x` is -10, crossing the lower bound -5.

---

    Code
      range_check_func(x, 0, 100, slack_prop = c(0.1, 0.05))
    Condition
      Error in `range_check_func()`:
      i Largest value of `x` is 110, crossing the upper bound 105.

---

    Code
      range_check_func(x, 0, 100, warn = TRUE)
    Condition
      Warning:
      i Smallest value of `x` is -10, crossing the lower bound -5.
      i Largest value of `x` is 110, crossing the upper bound 105.

# in recipe

    Code
      bake(rec2, test)
    Condition
      Error in `range_check_func()`:
      i Smallest value of `x` is -10, crossing the lower bound -5.
      i Largest value of `x` is 110, crossing the upper bound 105.

---

    Code
      bake(rec3, test)
    Condition
      Warning:
      i Smallest value of `x` is -10, crossing the lower bound -5.
      i Largest value of `x` is 110, crossing the upper bound 105.
      Warning:
      i Smallest value of `y` is -10, crossing the lower bound -2.5.
      i Largest value of `y` is 60, crossing the upper bound 52.5.
    Output
      # A tibble: 2 x 2
            x     y
        <dbl> <dbl>
      1   -10   -10
      2   110    60

---

    Code
      bake(rec4, test)
    Condition
      Error in `range_check_func()`:
      i Largest value of `y` is 60, crossing the upper bound 55.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(rec_trained, new_data = mtcars[, -3])
    Condition
      Error in `check_range()`:
      ! The following required column is missing from `new_data`: disp.

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
      * Checking range of: <none>

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
      * Checking range of: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      undeclared role: 11
      
      -- Operations 
      * Checking range of: drat, cyl, am

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      undeclared role: 11
      
      -- Training information 
      Training data contained 32 data points and no incomplete rows.
      
      -- Operations 
      * Checking range of: drat, cyl, am | Trained

