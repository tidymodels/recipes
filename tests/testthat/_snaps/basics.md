# Recipe fails on in-line functions

    Code
      recipe(HHV ~ log(nitrogen), data = biomass)
    Condition
      Error in `inline_check()`:
      ! No in-line functions should be used here; use steps to define baking actions.

---

    Code
      recipe(HHV ~ (.)^2, data = biomass)
    Condition
      Error in `inline_check()`:
      ! No in-line functions should be used here; use steps to define baking actions.

---

    Code
      recipe(HHV ~ nitrogen + sulfur + nitrogen:sulfur, data = biomass)
    Condition
      Error in `inline_check()`:
      ! No in-line functions should be used here; use steps to define baking actions.

---

    Code
      recipe(HHV ~ nitrogen^2, data = biomass)
    Condition
      Error in `inline_check()`:
      ! No in-line functions should be used here; use steps to define baking actions.

# Using prepare

    Code
      prepare(recipe(HHV ~ ., data = biomass), training = biomass)
    Condition
      Error in `prepare()`:
      ! As of version 0.0.1.9006, used `prep` instead of `prepare`

# bake without prep

    Code
      bake(sp_signed, new_data = biomass_te)
    Condition
      Error in `bake()`:
      ! At least one step has not been trained. Please run `prep`.

---

    Code
      juice(sp_signed)
    Condition
      Error in `juice()`:
      ! At least one step has not been trained. Please run `prep()`.

# bake without newdata

    Code
      bake(rec, newdata = biomass)
    Condition
      Error in `bake()`:
      ! 'new_data' must be either a data frame or NULL. No value is not allowed.

# tunable arguments at prep-time

    Code
      recipe(Species ~ ., data = iris) %>% step_ns(all_predictors(), deg_free = .tune()) %>%
        prep()
    Condition
      Error in `prep()`:
      ! You cannot `prep()` a tuneable recipe. Argument(s) with `tune()`: 'deg_free'. Do you want to use a tuning function such as `tune_grid()`?

# logging

    Code
      recipe(mpg ~ ., data = mtcars) %>% step_ns(disp, deg_free = 2, id = "splines!") %>%
        prep(log_changes = TRUE)
    Output
      step_ns (splines!): 
       new (2): disp_ns_1, disp_ns_2
       removed (1): disp
      
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor         10
      
      Training data contained 32 data points and no missing data.
      
      Operations:
      
      Natural splines on disp [trained]

# case weights are being infered correctly for formula interface

    Code
      recipe(mpg ~ cyl + disp, data = mtcars2)
    Condition
      Error in `too_many_case_weights()`:
      ! There should only be a single column with the role 'case_weights'. In these data, there are 2 columns.

# case weights are being infered correctly for x interface

    Code
      recipe(mtcars2)
    Condition
      Error in `too_many_case_weights()`:
      ! There should only be a single column with the role 'case_weights'. In these data, there are 2 columns.

# verbose when printing

    Code
      tmp <- prep(standardized, verbose = TRUE)
    Output
      oper 1 step center [training] 
      oper 2 step scale [training] 
      oper 3 step normalize [training] 
      The retained training set is ~ 0 Mb  in memory.
      

# `internal data is kept as tibbles when prepping

    Code
      bake(rec_prepped, new_data = as_tibble(mtcars))
    Condition
      Error in `bake()`:
      ! bake() methods should always return tibbles

---

    Code
      prep(rec_spec)
    Condition
      Error in `prep()`:
      ! bake() methods should always return tibbles

# recipe() errors if `data` is missing

    Code
      recipe(mpg ~ .)
    Condition
      Error in `recipe()`:
      ! Argument `data` is missing, with no default.

