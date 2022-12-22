# core function - correct input

    Code
      range_check_func(as.character(x), -10, 110)
    Condition
      Error in `range_check_func()`:
      ! is.numeric(x) is not TRUE

---

    Code
      range_check_func(x, -10, 110, "a")
    Condition
      Error in `range_check_func()`:
      ! is.numeric(slack_prop) is not TRUE

---

    Code
      range_check_func(x, -10, 110, c(0.05, 0.08, 0.05))
    Condition
      Error in `range_check_func()`:
      ! slack_prop should be of length 1 or of length 2

# core function - workings

    Code
      range_check_func(x, 0, 100)
    Condition
      Error in `range_check_func()`:
      ! min x is -10, lower bound is -5, max x is 110, upper bound is 105

---

    Code
      range_check_func(x, 0, 110)
    Condition
      Error in `range_check_func()`:
      ! min x is -10, lower bound is -5.5

---

    Code
      range_check_func(x, -5, 100)
    Condition
      Error in `range_check_func()`:
      ! max x is 110, upper bound is 105.25

---

    Code
      range_check_func(x, 0, 100, slack_prop = c(0.05, 0.1))
    Condition
      Error in `range_check_func()`:
      ! min x is -10, lower bound is -5

---

    Code
      range_check_func(x, 0, 100, slack_prop = c(0.1, 0.05))
    Condition
      Error in `range_check_func()`:
      ! max x is 110, upper bound is 105

---

    Code
      range_check_func(x, 0, 100, warn = TRUE)
    Condition
      Warning:
      min x is -10, lower bound is -5, max x is 110, upper bound is 105

# in recipe

    Code
      bake(rec2, test)
    Condition
      Error in `range_check_func()`:
      ! min x is -10, lower bound is -5, max x is 110, upper bound is 105

---

    Code
      bake(rec3, test)
    Condition
      Warning:
      min x is -10, lower bound is -5, max x is 110, upper bound is 105
      Warning:
      min y is -10, lower bound is -2.5, max x is 60, upper bound is 52.5
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
      ! max y is 60, upper bound is 55

# printing

    Code
      print(check_range_extract)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      undeclared role: 11
      
      -- Operations 
      * Checking range of: drat, cyl, am

---

    Code
      prep(check_range_extract)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      undeclared role: 11
      
      -- Training information 
      Training data contained 32 data points and no incomplete rows.
      
      -- Operations 
      * Checking range of: drat, cyl, am | Trained

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

