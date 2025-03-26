# No ISOmap

    Code
      print(im_rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 3
      
      -- Training information 
      Training data contained 5 data points and no incomplete rows.
      
      -- Operations 
      * Isomap was not conducted for: x1, x2, x3 | Trained

# ISOmap fails gracefully

    Code
      recipe(Sepal.Length ~ ., data = iris) %>% step_bs(Sepal.Width, deg_free = 1,
        degree = 1) %>% step_bs(Sepal.Length, deg_free = 1, degree = 1) %>%
        step_other(Species, threshold = 1e-09) %>% step_isomap(all_numeric_predictors(),
      num_terms = 1, neighbors = 1) %>% prep()
    Message
    Condition
      Error in `step_isomap()`:
      Caused by error in `prep()`:
      x Failed with error:
      i Error : TridiagEigen: eigen decomposition failed

# check_name() is used

    Code
      prep(rec, training = dat)
    Message
    Condition
      Error in `step_isomap()`:
      Caused by error in `bake()`:
      ! Name collision occurred. The following variable names already exist:
      * `Isomap1`

# bake method errors when needed non-standard role columns are missing

    Code
      bake(im_trained, new_data = dat2[, 1:2])
    Condition
      Error in `step_isomap()`:
      ! The following required column is missing from `new_data`: x3.

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
      * Isomap approximation with: <none>

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
      * Isomap approximation with: <none> | Trained

# keep_original_cols - can prep recipes with it missing

    Code
      rec <- prep(rec)
    Message
    Condition
      Warning:
      `keep_original_cols` was added to `step_isomap()` after this recipe was created.
      i Regenerate your recipe to avoid this warning.

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 3
      
      -- Operations 
      * Isomap approximation with: x1, x2, x3

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 3
      
      -- Training information 
      Training data contained 5 data points and no incomplete rows.
      
      -- Operations 
      * Isomap approximation with: x1, x2, x3 | Trained

# bad args

    Code
      recipe(~., data = mtcars) %>% step_isomap(all_predictors(), num_terms = 2,
      neighbors = -1 / 3) %>% prep()
    Condition
      Error in `step_isomap()`:
      Caused by error in `prep()`:
      ! `neighbors` must be a whole number, not the number -0.33.

---

    Code
      recipe(~., data = mtcars) %>% step_isomap(all_predictors(), prefix = NULL) %>%
        prep()
    Condition
      Error in `step_isomap()`:
      Caused by error in `prep()`:
      ! `prefix` must be a single string, not `NULL`.

