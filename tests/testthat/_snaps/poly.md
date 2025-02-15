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
      res <- recipe(~., data = iris) %>% step_poly(Sepal.Width, options = list(
        degree = 3)) %>% prep() %>% bake(new_data = NULL)
    Message
      The `degree` argument is now a main argument instead of being within `options`.

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
      * Orthogonal polynomials on: <none>

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
      * Orthogonal polynomials on: <none> | Trained

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
      * Orthogonal polynomials on: carbon hydrogen

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
      * Orthogonal polynomials on: carbon hydrogen | Trained

# bad args

    Code
      recipe(mpg ~ ., data = mtcars) %>% step_poly(disp, degree = 0) %>% prep()
    Condition
      Error in `step_poly()`:
      Caused by error in `prep()`:
      ! `degree` must be a whole number larger than or equal to 1, not the number 0.

