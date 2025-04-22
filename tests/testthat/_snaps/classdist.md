# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_classdist()`:
      Caused by error in `bake()`:
      ! Name collision occurred. The following variable names already exist:
      * `classdist_setosa`

# case weights

    Code
      recipes:::get_center(mtcars, wts = wts, mfun = median)
    Condition
      Error in `recipes:::get_center()`:
      ! The centering function requested cannot be used with case weights.

---

    Code
      recipes:::get_both(mtcars, wts = wts, mfun = median)
    Condition
      Error in `recipes:::get_both()`:
      ! The centering function requested cannot be used with case weights.

---

    Code
      recipes:::get_both(mtcars, wts = wts, cfun = mad)
    Condition
      Error in `recipes:::get_both()`:
      ! The variance function requested cannot be used with case weights.

---

    Code
      rec_prep
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:      1
      predictor:    4
      case_weights: 1
      
      -- Training information 
      Training data contained 150 data points and no incomplete rows.
      
      -- Operations 
      * Distances to Species for: Sepal.Length, ... | Trained, weighted

# recipes_argument_select() is used

    Code
      prep(step_classdist(recipe(mpg ~ ., data = mtcars), disp, class = NULL))
    Condition
      Error in `step_classdist()`:
      Caused by error in `prep()`:
      ! `class` must not be `NULL`.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(trained, new_data = iris[, c(-3)])
    Condition
      Error in `step_classdist()`:
      ! The following required column is missing from `new_data`: Petal.Length.

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
      * Distances to Species for: <none>

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
      * Distances to Species for: <none> | Trained

# keep_original_cols - can prep recipes with it missing

    Code
      rec <- prep(rec)
    Condition
      Warning:
      `keep_original_cols` was added to `step_classdist()` after this recipe was created.
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
      * Distances to Species for: all_predictors()

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
      * Distances to Species for: Sepal.Length Sepal.Width, ... | Trained

# bad args

    Code
      prep(step_classdist(recipe(Species ~ ., data = iris), all_predictors(), class = Species,
      mean_func = 2))
    Condition
      Error in `step_classdist()`:
      Caused by error in `prep()`:
      ! `x$mean_func` must be a function, not the number 2.

---

    Code
      prep(step_classdist(recipe(Species ~ ., data = iris), all_predictors(), class = Species,
      cov_func = NULL))
    Condition
      Error in `step_classdist()`:
      Caused by error in `prep()`:
      ! `x$cov_func` must be a function, not `NULL`.

---

    Code
      prep(step_classdist(recipe(Species ~ ., data = iris), all_predictors(), class = Species,
      prefix = NULL))
    Condition
      Error in `step_classdist()`:
      Caused by error in `prep()`:
      ! `x$prefix` must be a single string, not `NULL`.

---

    Code
      prep(step_classdist(recipe(Species ~ ., data = iris), all_predictors(), class = Species,
      pool = NULL))
    Condition
      Error in `step_classdist()`:
      Caused by error in `prep()`:
      ! `x$pool` must be `TRUE` or `FALSE`, not `NULL`.

