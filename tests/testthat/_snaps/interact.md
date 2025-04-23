# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_interact()`:
      Caused by error in `bake()`:
      ! Name collision occurred. The following variable names already exist:
      * `x1ax2`

# gives informative error if terms isn't a formula (#1299)

    Code
      prep(step_interact(recipe(mpg ~ ., data = mtcars), terms = starts_with("dis")))
    Condition
      Error in `step_interact()`:
      Caused by error:
      ! `terms` must be supplied as a formula.

---

    Code
      tmp <- prep(step_interact(recipe(mpg ~ ., data = mtcars, strings_as_factors = FALSE),
      ~ disp:am))
    Condition
      Warning:
      Categorical variables used in `step_interact()` should probably be avoided; This can lead to differences in dummy variable values that are produced by ?step_dummy (`?recipes::step_dummy()`). Please convert all involved variables to dummy variables first.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(int_rec_trained, dat_tr[, 4:6])
    Condition
      Error in `step_interact()`:
      ! The following required columns are missing from `new_data`: z and x1.

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
      * Interactions with: <none>

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
      * Interactions with: <none> | Trained

# keep_original_cols - can prep recipes with it missing

    Code
      rec <- prep(rec)
    Condition
      Warning:
      `keep_original_cols` was added to `step_interact()` after this recipe was created.
      i Regenerate your recipe to avoid this warning.

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 6
      
      -- Operations 
      * Interactions with: x1:x2

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 6
      
      -- Training information 
      Training data contained 10 data points and no incomplete rows.
      
      -- Operations 
      * Interactions with: x1:x2 | Trained

# bad args

    Code
      prep(step_interact(recipe(mpg ~ ., data = mtcars), ~ disp::wt, sep = TRUE))
    Condition
      Error in `step_interact()`:
      ! `sep` must be a single string, not `TRUE`.

