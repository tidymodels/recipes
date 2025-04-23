# missing data

    Code
      prep(rec_false, training = ex_dat, verbose = FALSE)
    Condition
      Error in `step_YeoJohnson()`:
      Caused by error in `prep()`:
      x Missing values are not allowed for the YJ transformation.
      i See `na_rm` option.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(rec_trained, new_data = ex_dat[, 1:2])
    Condition
      Error in `step_YeoJohnson()`:
      ! The following required column is missing from `new_data`: x4.

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
      * Yeo-Johnson transformation on: <none>

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
      * Yeo-Johnson transformation on: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 4
      
      -- Operations 
      * Yeo-Johnson transformation on: x1, x2, x3, x4

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 4
      
      -- Training information 
      Training data contained 20 data points and no incomplete rows.
      
      -- Operations 
      * Yeo-Johnson transformation on: x1, x2, x4 | Trained

# bad args

    Code
      prep(step_YeoJohnson(recipe(~., data = ex_dat), x1, x2, x3, x4, na_rm = "yes"))
    Condition
      Error in `step_YeoJohnson()`:
      Caused by error in `prep()`:
      ! `na_rm` must be `TRUE` or `FALSE`, not the string "yes".

---

    Code
      prep(step_YeoJohnson(recipe(~., data = ex_dat), x1, x2, x3, x4, num_unique = "yes"))
    Condition
      Error in `step_YeoJohnson()`:
      Caused by error in `prep()`:
      ! `x$num_unique` must be a whole number, not the string "yes".

---

    Code
      prep(step_YeoJohnson(recipe(~., data = ex_dat), x1, x2, x3, x4, limits = NA_real_))
    Condition
      Error in `step_YeoJohnson()`:
      Caused by error in `prep()`:
      ! `limits` should be a numeric vector with two values, not a double vector

