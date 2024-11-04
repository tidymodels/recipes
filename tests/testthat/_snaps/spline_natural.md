# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_spline_natural()`:
      Caused by error in `bake()`:
      ! Name collision occurred. The following variable names already exist:
      * `mpg_01`

# bake method errors when needed non-standard role columns are missing

    Code
      bake(rec_trained, new_data = mtcars[, -3])
    Condition
      Error in `step_spline_natural()`:
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
      * Natural spline expansion: <none>

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
      * Natural spline expansion: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 5
      
      -- Operations 
      * Natural spline expansion: carbon and hydrogen

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 5
      
      -- Training information 
      Training data contained 536 data points and no incomplete rows.
      
      -- Operations 
      * Natural spline expansion: carbon and hydrogen | Trained

# bad args

    Code
      recipe(mpg ~ ., data = mtcars) %>% step_spline_natural(disp, deg_free = "a") %>%
        prep()
    Condition
      Error in `step_spline_natural()`:
      Caused by error in `prep()`:
      ! `deg_free` must be a whole number, not the string "a".

---

    Code
      recipe(mpg ~ ., data = mtcars) %>% step_spline_natural(disp, complete_set = 1) %>%
        prep()
    Condition
      Error in `step_spline_natural()`:
      Caused by error in `prep()`:
      ! `complete_set` must be `TRUE` or `FALSE`, not the number 1.

