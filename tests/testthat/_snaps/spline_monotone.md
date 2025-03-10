# errors if degree > deg_free (#1170)

    Code
      recipe(~., data = mtcars) %>% step_spline_monotone(mpg, degree = 3, deg_free = 3,
        complete_set = TRUE) %>% prep()
    Condition
      Error in `step_spline_monotone()`:
      Caused by error in `prep()`:
      ! `degree` (3) must be less than to `deg_free` (3) when `complete_set = FALSE`.

---

    Code
      recipe(~., data = mtcars) %>% step_spline_monotone(mpg, degree = 4, deg_free = 3,
        complete_set = FALSE) %>% prep()
    Condition
      Error in `step_spline_monotone()`:
      Caused by error in `prep()`:
      ! `degree` (4) must be less than or equal to `deg_free` (3) when `complete_set = TRUE`.

# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_spline_monotone()`:
      Caused by error in `bake()`:
      ! Name collision occurred. The following variable names already exist:
      * `mpg_01`

# bake method errors when needed non-standard role columns are missing

    Code
      bake(rec_trained, new_data = mtcars[, -3])
    Condition
      Error in `step_spline_monotone()`:
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
      * Monotone spline expansion: <none>

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
      * Monotone spline expansion: <none> | Trained

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
      * Monotone spline expansion: carbon hydrogen

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
      * Monotone spline expansion: carbon hydrogen | Trained

# bad args

    Code
      recipe(mpg ~ ., data = mtcars) %>% step_spline_monotone(disp, degree = -1) %>%
        prep()
    Condition
      Error in `step_spline_monotone()`:
      Caused by error in `prep()`:
      ! `degree` must be a whole number larger than or equal to 0, not the number -1.

---

    Code
      recipe(mpg ~ ., data = mtcars) %>% step_spline_monotone(disp, deg_free = "a") %>%
        prep()
    Condition
      Error in `step_spline_monotone()`:
      Caused by error in `prep()`:
      ! `deg_free` must be a whole number, not the string "a".

---

    Code
      recipe(mpg ~ ., data = mtcars) %>% step_spline_monotone(disp, complete_set = 1) %>%
        prep()
    Condition
      Error in `step_spline_monotone()`:
      Caused by error in `prep()`:
      ! `complete_set` must be `TRUE` or `FALSE`, not the number 1.

