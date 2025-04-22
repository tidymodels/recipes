# dummy variables errors with character inputs

    Code
      prep(dummy, training = sacr, verbose = FALSE)
    Condition
      Error in `step_dummy()`:
      Caused by error in `prep()`:
      x All columns selected for the step should be factor or ordered.
      * 2 string variables found: `city` and `zip`

# check_type() is used

    Code
      prep(step_dummy(recipe(sqft ~ zip + price + city, data = sacr), city, zip,
      price))
    Condition
      Error in `step_dummy()`:
      Caused by error in `prep()`:
      x All columns selected for the step should be factor or ordered.
      * 1 integer variable found: `price`

# make sure contrasts argument is checked

    Code
      prep(step_dummy(recipe(~Species, iris), Species, contrasts = TRUE))
    Condition
      Error in `step_dummy()`:
      Caused by error in `prep()`:
      ! The ordered element of `contracts` must be a string, not `TRUE`.

---

    Code
      prep(step_dummy(recipe(~Species, iris), Species, contrasts = list()))
    Condition
      Error in `step_dummy()`:
      Caused by error in `prep()`:
      ! The list passed to `contrasts` must have the names "ordered" and "unordered".

---

    Code
      prep(step_dummy(recipe(~Species, iris), Species, contrasts = list(ordered = "contr.treatment")))
    Condition
      Error in `step_dummy()`:
      Caused by error in `prep()`:
      ! The ordered element of `contracts` must be a string, not a list.

---

    Code
      prep(step_dummy(recipe(~Species, iris), Species, contrasts = list(ordered = 1,
        unordered = "contr.treatment")))
    Condition
      Error in `step_dummy()`:
      Caused by error in `prep()`:
      ! The ordered element of `contracts` must be a string, not a number.

---

    Code
      prep(step_dummy(recipe(~Species, iris), Species, contrasts = list(ordered = "contr.treatment",
        unordered = 1)))
    Condition
      Error in `step_dummy()`:
      Caused by error in `prep()`:
      ! The unordered element of `contracts` must be a string, not a number.

# getOption('contrasts') gives deprecation warning in step_dummy

    Code
      tmp <- prep(step_dummy(recipe(~., data = iris), Species))
    Condition
      Warning:
      options(contrasts) with step_dummy() was deprecated in recipes 1.3.0.
      i Please use step_dummy(contrasts) instead.

# tests for NA values in factor

    Code
      factors <- prep(factors, training = sacr_missing)
    Condition
      Warning:
      ! There are new levels in `city`: NA.
      i Consider using step_unknown() (`?recipes::step_unknown()`) before `step_dummy()` to handle missing values.

---

    Code
      factors_data_1 <- bake(factors, new_data = sacr_missing)
    Condition
      Warning:
      ! There are new levels in `city`: NA.
      i Consider using step_unknown() (`?recipes::step_unknown()`) before `step_dummy()` to handle missing values.

# tests for NA values in ordered factor

    Code
      factors <- prep(factors, training = sacr_ordered)
    Condition
      Warning:
      ! There are new levels in `city`: NA.
      i Consider using step_unknown() (`?recipes::step_unknown()`) before `step_dummy()` to handle missing values.

---

    Code
      factors_data_1 <- bake(factors, new_data = sacr_ordered)
    Condition
      Warning:
      ! There are new levels in `city`: NA.
      i Consider using step_unknown() (`?recipes::step_unknown()`) before `step_dummy()` to handle missing values.

# new levels

    Code
      recipes:::warn_new_levels(testing$x1, levels(training$x1), "column",
      "step_dummy")
    Condition
      Warning:
      ! There are new levels in `column`: C.
      i Consider using step_novel() (`?recipes::step_novel()`) before `step_dummy()` to handle unseen values.

---

    Code
      bake(rec, new_data = testing)
    Condition
      Warning:
      ! There are new levels in `x1`: C.
      i Consider using step_novel() (`?recipes::step_novel()`) before `step_dummy()` to handle unseen values.
    Output
      # A tibble: 10 x 2
         y      x1_B
         <fct> <dbl>
       1 0         0
       2 0         1
       3 1         0
       4 0        NA
       5 1        NA
       6 0         1
       7 0         0
       8 0         1
       9 1        NA
      10 0         0

# warns about NA in column (#450)

    Code
      tmp <- prep(step_dummy(recipe(~a, data = data), a))
    Condition
      Warning:
      ! There are new levels in `a`: NA.
      i Consider using step_unknown() (`?recipes::step_unknown()`) before `step_dummy()` to handle missing values.

# Deprecation warning

    Code
      step_dummy(recipe(~., data = mtcars), preserve = TRUE)
    Condition
      Error:
      ! The `preserve` argument of `step_dummy()` was deprecated in recipes 0.1.16 and is now defunct.
      i Please use the `keep_original_cols` argument instead.

# no columns selected

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 2
      
      -- Training information 
      Training data contained 3 data points and no incomplete rows.
      
      -- Operations 
      * Zero variance filter removed: x | Trained
      * Dummy variables from: <none> | Trained

# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_dummy()`:
      Caused by error in `bake()`:
      ! Name collision occurred. The following variable names already exist:
      * `Species_versicolor`

# throws a informative error for too many levels (#828)

    Code
      prep(rec)
    Condition
      Error in `step_dummy()`:
      Caused by error:
      ! `x` contains too many levels (123456), which would result in a data.frame too large to fit in memory.

# throws an informative error for single level

    Code
      prep(step_dummy(recipe(~., data = data.frame(x = "only-level")), x))
    Condition
      Error in `step_dummy()`:
      Caused by error in `bake()`:
      ! Only one factor level in `x`: "only-level".

# bake method errors when needed non-standard role columns are missing

    Code
      bake(dummy_trained, new_data = sacr_fac[, 3:4], all_predictors())
    Condition
      Error in `step_dummy()`:
      ! The following required columns are missing from `new_data`: city and zip.

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
      * Dummy variables from: <none>

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
      * Dummy variables from: <none> | Trained

# keep_original_cols - can prep recipes with it missing

    Code
      rec <- prep(rec)
    Condition
      Warning:
      `keep_original_cols` was added to `step_dummy()` after this recipe was created.
      i Regenerate your recipe to avoid this warning.

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 7
      
      -- Operations 
      * Dummy variables from: city zip

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 7
      
      -- Training information 
      Training data contained 932 data points and no incomplete rows.
      
      -- Operations 
      * Dummy variables from: city zip | Trained

# bad args

    Code
      prep(step_dummy(recipe(~ city + sqft + price, data = Sacramento), city,
      one_hot = 2))
    Condition
      Error in `step_dummy()`:
      Caused by error in `prep()`:
      ! `one_hot` must be `TRUE` or `FALSE`, not the number 2.

---

    Code
      prep(step_dummy(recipe(~ city + sqft + price, data = Sacramento), city, naming = NULL))
    Condition
      Error in `step_dummy()`:
      Caused by error in `prep()`:
      ! `naming` must be a function, not `NULL`.

