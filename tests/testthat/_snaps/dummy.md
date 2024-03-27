# dummy variables errors with character inputs

    Code
      prep(dummy, training = sacr, verbose = FALSE, strings_as_factors = FALSE)
    Condition
      Error in `step_dummy()`:
      Caused by error in `prep()`:
      x All columns selected for the step should be factor or ordered.
      * 2 string variables found: `city` and `zip`

# check_type() is used

    Code
      recipe(sqft ~ zip + price + city, data = sacr) %>% step_dummy(city, zip, price) %>%
        prep()
    Condition
      Error in `step_dummy()`:
      Caused by error in `prep()`:
      x All columns selected for the step should be factor or ordered.
      * 1 integer variable found: `price`

# tests for NA values in factor

    Code
      factors <- prep(factors, training = sacr_missing)
    Condition
      Warning:
      ! There are new levels in a factor: `NA`.

---

    Code
      factors_data_1 <- bake(factors, new_data = sacr_missing)
    Condition
      Warning:
      ! There are new levels in a factor: `NA`.

# tests for NA values in ordered factor

    Code
      factors <- prep(factors, training = sacr_ordered)
    Condition
      Warning:
      ! There are new levels in a factor: `NA`.

---

    Code
      factors_data_1 <- bake(factors, new_data = sacr_ordered)
    Condition
      Warning:
      ! There are new levels in a factor: `NA`.

# new levels

    Code
      recipes:::warn_new_levels(testing$x1, levels(training$x1))
    Condition
      Warning:
      ! There are new levels in a factor: `C`.

---

    Code
      bake(rec, new_data = testing)
    Condition
      Warning:
      ! There are new levels in a factor: `C`.
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

# Deprecation warning

    Code
      recipe(~., data = mtcars) %>% step_dummy(preserve = TRUE)
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
      * Dummy variables from: city and zip

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
      * Dummy variables from: city and zip | Trained

