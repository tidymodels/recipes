# errors if degree > deg_free (#1170)

    Code
      prep(step_spline_convex(recipe(~., data = mtcars), mpg, degree = 3, deg_free = 3,
      complete_set = TRUE))
    Condition
      Error in `step_spline_convex()`:
      Caused by error in `prep()`:
      ! `degree` (3) must be less than to `deg_free` (3) when `complete_set = FALSE`.

---

    Code
      prep(step_spline_convex(recipe(~., data = mtcars), mpg, degree = 4, deg_free = 3,
      complete_set = FALSE))
    Condition
      Error in `step_spline_convex()`:
      Caused by error in `prep()`:
      ! `degree` (4) must be less than or equal to `deg_free` (3) when `complete_set = TRUE`.

# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_spline_convex()`:
      Caused by error in `bake()`:
      ! Name collision occurred. The following variable names already exist:
      * `mpg_01`

# errors with zero variance predictors (#1455)

    Code
      prep(step_spline_convex(recipe(mpg ~ ., data = mtcars), all_numeric_predictors()))
    Condition
      Error in `step_spline_convex()`:
      Caused by error in `prep()`:
      ! The following columns have zero variance making computations unable to proceed: disp and vs.
      i Consider using ?step_zv (`?recipes::step_zv()`) to remove those columns before this step.

# check_options() is used

    Code
      prep(step_spline_convex(recipe(~mpg, data = mtcars), mpg, options = TRUE))
    Condition
      Error in `step_spline_convex()`:
      Caused by error in `prep()`:
      ! `options` must be a list, not `TRUE`.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(rec_trained, new_data = mtcars[, -3])
    Condition
      Error in `step_spline_convex()`:
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
      * Convex spline expansion: <none>

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
      * Convex spline expansion: <none> | Trained

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
      * Convex spline expansion: carbon hydrogen

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
      * Convex spline expansion: carbon hydrogen | Trained

# bad args

    Code
      prep(step_spline_convex(recipe(mpg ~ ., data = mtcars), disp, degree = -1))
    Condition
      Error in `step_spline_convex()`:
      Caused by error in `prep()`:
      ! `degree` must be a whole number larger than or equal to 0, not the number -1.

---

    Code
      prep(step_spline_convex(recipe(mpg ~ ., data = mtcars), disp, complete_set = 1))
    Condition
      Error in `step_spline_convex()`:
      Caused by error in `prep()`:
      ! `complete_set` must be `TRUE` or `FALSE`, not the number 1.

