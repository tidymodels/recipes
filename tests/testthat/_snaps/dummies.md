# dummy variables with non-factor inputs

    Code
      prep(dummy, training = okc, verbose = FALSE, strings_as_factors = FALSE)
    Condition
      Warning:
      The following variables are not factor vectors and will be ignored: `diet`, `location`
      Error in `check_factor_vars()`:
      ! The `terms` argument in `step_dummy` did not select any factor columns.

---

    Code
      recipe(age ~ location + height + diet, data = okc_fac_ish) %>% step_dummy(diet,
        location, height) %>% prep(training = okc_fac_ish, verbose = FALSE,
        strings_as_factors = FALSE)
    Condition
      Warning:
      The following variables are not factor vectors and will be ignored: `diet`, `height`
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor          3
      
      Training data contained 59853 data points and no missing data.
      
      Operations:
      
      Dummy variables from location [trained]

# tests for NA values in factor

    Code
      factors <- prep(factors, training = okc_missing)
    Condition
      Warning:
      There are new levels in a factor: NA

---

    Code
      factors_data_1 <- bake(factors, new_data = okc_missing)
    Condition
      Warning:
      There are new levels in a factor: NA

# tests for NA values in ordered factor

    Code
      factors <- prep(factors, training = okc_ordered)
    Condition
      Warning:
      There are new levels in a factor: NA

---

    Code
      factors_data_1 <- bake(factors, new_data = okc_ordered)
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

# printing

    Code
      print(dummy)
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor          4
      
      Operations:
      
      Dummy variables from diet, location

---

    Code
      prep(dummy, training = okc_fac, verbose = TRUE)
    Output
      oper 1 step dummy [training] 
      The retained training set is ~ 70.29 Mb  in memory.
      
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor          4
      
      Training data contained 59853 data points and no missing data.
      
      Operations:
      
      Dummy variables from diet, location [trained]

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
      dummy_trained <- prep(dummy, training = okc_fac, verbose = FALSE)
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

