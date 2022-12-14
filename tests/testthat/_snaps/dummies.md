# dummy variables with non-factor inputs

    Code
      prep(dummy, training = sacr, verbose = FALSE, strings_as_factors = FALSE)
    Condition
      Warning:
      The following variables are not factor vectors and will be ignored: `city`, `zip`
      Error in `step_dummy()`:
      Caused by error in `prep()`:
      ! The `terms` argument in `step_dummy` did not select any factor columns.

---

    Code
      recipe(sqft ~ zip + price + city, data = sacr_fac_ish) %>% step_dummy(city, zip,
        price) %>% prep(training = sacr_fac_ish, verbose = FALSE, strings_as_factors = FALSE)
    Condition
      Error in `step_dummy()`:
      Caused by error in `prep()`:
      ! All columns selected for the step should be string, factor, or ordered.

# tests for NA values in factor

    Code
      factors <- prep(factors, training = sacr_missing)
    Condition
      Warning:
      There are new levels in a factor: NA

---

    Code
      factors_data_1 <- bake(factors, new_data = sacr_missing)
    Condition
      Warning:
      There are new levels in a factor: NA

# tests for NA values in ordered factor

    Code
      factors <- prep(factors, training = sacr_ordered)
    Condition
      Warning:
      There are new levels in a factor: NA

---

    Code
      factors_data_1 <- bake(factors, new_data = sacr_ordered)
    Condition
      Warning:
      There are new levels in a factor: NA

# new levels

    Code
      recipes:::warn_new_levels(testing$x1, levels(training$x1))
    Condition
      Warning:
      There are new levels in a factor: C

---

    Code
      bake(rec, new_data = testing)
    Condition
      Warning:
      There are new levels in a factor: C
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

# printing

    Code
      print(dummy)
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor          7
      
      Operations:
      
      Dummy variables from city, zip

---

    Code
      prep(dummy)
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor          7
      
      Training data contained 932 data points and no missing data.
      
      Operations:
      
      Dummy variables from city, zip [trained]

# no columns selected

    Code
      print(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor          2
      
      Training data contained 3 data points and no missing data.
      
      Operations:
      
      Zero variance filter removed x [trained]
      Dummy variables from <none> [trained]

# can prep recipes with no keep_original_cols

    Code
      dummy_trained <- prep(dummy, training = sacr_fac, verbose = FALSE)
    Condition
      Warning:
      'keep_original_cols' was added to `step_dummy()` after this recipe was created.
      Regenerate your recipe to avoid this warning.

# empty printing

    Code
      rec
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor         10
      
      Operations:
      
      Dummy variables from <none>

---

    Code
      rec
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor         10
      
      Training data contained 32 data points and no missing data.
      
      Operations:
      
      Dummy variables from <none> [trained]

