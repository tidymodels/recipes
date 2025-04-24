# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_poly()`:
      Caused by error in `bake()`:
      ! Name collision occurred. The following variable names already exist:
      * `mpg_poly_1`

# old option argument

    Code
      res <- bake(prep(step_poly(recipe(~., data = iris), Sepal.Width, options = list(
        degree = 3))), new_data = NULL)
    Message
      The `degree` argument is now a main argument instead of being within `options`.

# check_options() is used

    Code
      prep(step_poly(recipe(~mpg, data = mtcars), mpg, options = TRUE))
    Condition
      Error in `step_poly()`:
      Caused by error in `prep()`:
      ! `options` must be a list, not `TRUE`.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(with_poly, new_data = biomass_tr[, c(-3)])
    Condition
      Error in `step_poly()`:
      ! The following required column is missing from `new_data`: carbon.

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
      * Polynomial expansion on: <none>

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
      * Polynomial expansion on: <none> | Trained

# keep_original_cols - can prep recipes with it missing

    Code
      rec <- prep(rec)
    Condition
      Warning:
      `keep_original_cols` was added to `step_poly()` after this recipe was created.
      i Regenerate your recipe to avoid this warning.

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
      * Polynomial expansion on: carbon hydrogen

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
      Training data contained 456 data points and no incomplete rows.
      
      -- Operations 
      * Polynomial expansion on: carbon hydrogen | Trained

# bad args

    Code
      prep(step_poly(recipe(mpg ~ ., data = mtcars), disp, degree = 0))
    Condition
      Error in `step_poly()`:
      Caused by error in `prep()`:
      ! `degree` must be a whole number larger than or equal to 1, not the number 0.

