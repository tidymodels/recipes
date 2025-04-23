# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_poly_bernstein()`:
      Caused by error in `bake()`:
      ! Name collision occurred. The following variable names already exist:
      * `mpg_01`

# check_options() is used

    Code
      prep(step_poly_bernstein(recipe(~mpg, data = mtcars), mpg, options = TRUE))
    Condition
      Error in `step_poly_bernstein()`:
      Caused by error in `prep()`:
      ! `options` must be a list, not `TRUE`.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(rec_trained, new_data = mtcars[, -3])
    Condition
      Error in `step_poly_bernstein()`:
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
      * Bernstein polynomial expansion: <none>

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
      * Bernstein polynomial expansion: <none> | Trained

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
      * Bernstein polynomial expansion: carbon hydrogen

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
      * Bernstein polynomial expansion: carbon hydrogen | Trained

# bad args

    Code
      prep(step_poly_bernstein(recipe(mpg ~ ., data = mtcars), disp, degree = -1))
    Condition
      Error in `step_poly_bernstein()`:
      Caused by error in `prep()`:
      ! `degree` must be a whole number larger than or equal to 0, not the number -1.

---

    Code
      prep(step_poly_bernstein(recipe(mpg ~ ., data = mtcars), disp, complete_set = 1))
    Condition
      Error in `step_poly_bernstein()`:
      Caused by error in `prep()`:
      ! `complete_set` must be `TRUE` or `FALSE`, not the number 1.

