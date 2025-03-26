# bake method errors when needed non-standard role columns are missing

    Code
      bake(trained, new_data = iris[, 2:5])
    Condition
      Error in `step_depth()`:
      ! The following required column is missing from `new_data`: Sepal.Length.

# empty printing

    Code
      rec
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 4
      
      -- Operations 
      * Data depth by Species for: <none>

---

    Code
      rec
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 4
      
      -- Training information 
      Training data contained 150 data points and no incomplete rows.
      
      -- Operations 
      * Data depth by Species for: <none> | Trained

# keep_original_cols - can prep recipes with it missing

    Code
      rec <- prep(rec)
    Condition
      Warning:
      `keep_original_cols` was added to `step_depth()` after this recipe was created.
      i Regenerate your recipe to avoid this warning.

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 4
      
      -- Operations 
      * Data depth by Species for: all_predictors()

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 4
      
      -- Training information 
      Training data contained 150 data points and no incomplete rows.
      
      -- Operations 
      * Data depth by Species for: Sepal.Length Sepal.Width, ... | Trained

# bad args

    Code
      recipe(Species ~ ., data = iris) %>% step_depth(all_numeric_predictors(),
      class = "Species", metric = "circular") %>% prep()
    Condition
      Error in `step_depth()`:
      Caused by error in `prep()`:
      ! `metric` must be one of "potential", "halfspace", "Mahalanobis", "simplicialVolume", "spatial", or "zonoid", not "circular".

---

    Code
      recipe(Species ~ ., data = iris) %>% step_depth(all_numeric_predictors(),
      class = "Species", prefix = 0L) %>% prep()
    Condition
      Error in `step_depth()`:
      Caused by error in `prep()`:
      ! `prefix` must be a single string, not the number 0.

