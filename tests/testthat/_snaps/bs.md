# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_bs()`:
      Caused by error in `bake()`:
      ! Name collision occurred. The following variable names already exist:
      * `mpg_bs_1`

# check_options() is used

    Code
      prep(step_bs(recipe(mpg ~ ., data = mtcars), disp, options = TRUE))
    Condition
      Error in `step_bs()`:
      Caused by error in `prep()`:
      ! `options` must be a list, not `TRUE`.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(with_bs, new_data = biomass_tr[, c(-4)])
    Condition
      Error in `step_bs()`:
      ! The following required column is missing from `new_data`: hydrogen.

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
      * B-splines on: <none>

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
      * B-splines on: <none> | Trained

# keep_original_cols - can prep recipes with it missing

    Code
      rec <- prep(rec)
    Condition
      Warning:
      `keep_original_cols` was added to `step_bs()` after this recipe was created.
      i Regenerate your recipe to avoid this warning.

# printing

    Code
      print(with_bs)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 5
      
      -- Operations 
      * B-splines on: carbon hydrogen

---

    Code
      prep(with_bs)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 5
      
      -- Training information 
      Training data contained 456 data points and no incomplete rows.
      
      -- Operations 
      * B-splines on: carbon hydrogen | Trained

