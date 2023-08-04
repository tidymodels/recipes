# bad values

    Code
      sacr_rec %>% step_profile(everything(), profile = vars(sqft)) %>% prep(data = Sacramento)
    Condition
      Error in `step_profile()`:
      Caused by error in `prep()`:
      ! The profiled variable cannot be in the list of variables to be fixed.

---

    Code
      sacr_rec %>% step_profile(sqft, beds, price, profile = vars(zip, beds)) %>%
        prep(data = Sacramento)
    Condition
      Error in `step_profile()`:
      Caused by error in `prep()`:
      ! Only one variable should be profiled

---

    Code
      sacr_rec %>% step_profile(city, profile = vars(sqft), pct = -1) %>% prep(data = Sacramento)
    Condition
      Error in `step_profile()`:
      ! `pct should be on [0, 1]`

---

    Code
      sacr_rec %>% step_profile(city, profile = vars(sqft), grid = 1:3) %>% prep(
        data = Sacramento)
    Condition
      Error in `step_profile()`:
      ! `grid` should have two named elements. See ?step_profile

---

    Code
      sacr_rec %>% step_profile(city, profile = vars(sqft), grid = list(pctl = 1,
        len = 2)) %>% prep(data = Sacramento)
    Condition
      Error in `step_profile()`:
      ! `grid$pctl should be logical.`

---

    Code
      fixed(rep(c(TRUE, FALSE), each = 5))
    Condition
      Error in `error_cnd()`:
      ! Conditions must have named data fields

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

