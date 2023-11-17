# bad values

    Code
      sacr_rec %>% step_profile(everything(), profile = vars(sqft)) %>% prep(data = Sacramento)
    Condition
      Error in `step_profile()`:
      Caused by error in `prep()`:
      ! The profiled variable cannot be in the list of variables to be fixed. `sqft` was in both.

---

    Code
      sacr_rec %>% step_profile(sqft, beds, price, profile = vars(zip, beds)) %>%
        prep(data = Sacramento)
    Condition
      Error in `step_profile()`:
      Caused by error in `prep()`:
      x `profile` should select only one column
      i 2 columns were selected: `zip` and `beds`.

---

    Code
      sacr_rec %>% step_profile(city, profile = vars(sqft), pct = -1) %>% prep(data = Sacramento)
    Condition
      Error in `step_profile()`:
      ! `pct` must be a number between 0 and 1, not the number -1.

---

    Code
      sacr_rec %>% step_profile(city, profile = vars(sqft), grid = 1:3) %>% prep(
        data = Sacramento)
    Condition
      Error in `step_profile()`:
      x `grid` should have 2 elements, not 3.
      i See ?step_profile (`?recipes::step_profile()`) for information.

---

    Code
      sacr_rec %>% step_profile(city, profile = vars(sqft), grid = list(pctl = 1,
        len = 2)) %>% prep(data = Sacramento)
    Condition
      Error in `step_profile()`:
      ! `grid$pctl` must be `TRUE` or `FALSE`, not the number 1.

---

    Code
      fixed(rep(c(TRUE, FALSE), each = 5))
    Condition
      Error in `fixed()`:
      ! No method for determining a value to fix for objects of class: <logical>.

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

