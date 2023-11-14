# Recipe fails on in-line functions

    Code
      recipe(HHV ~ log(nitrogen), data = biomass)
    Condition
      Error in `inline_check()`:
      x No in-line functions should be used here.
      i The following function was found: `log`.
      i Use steps to do transformations instead.
      i If your modeling engine uses special terms in formulas, pass that formula to workflows as a model formula (`?parsnip::model_formula()`).

---

    Code
      recipe(HHV ~ (.)^2, data = biomass)
    Condition
      Error in `inline_check()`:
      x No in-line functions should be used here.
      i The following functions were found: `^` and `(`.
      i Use steps to do transformations instead.
      i If your modeling engine uses special terms in formulas, pass that formula to workflows as a model formula (`?parsnip::model_formula()`).

---

    Code
      recipe(HHV ~ nitrogen + sulfur + nitrogen:sulfur, data = biomass)
    Condition
      Error in `inline_check()`:
      x No in-line functions should be used here.
      i The following function was found: `:`.
      i Use steps to do transformations instead.
      i If your modeling engine uses special terms in formulas, pass that formula to workflows as a model formula (`?parsnip::model_formula()`).

---

    Code
      recipe(HHV ~ nitrogen^2, data = biomass)
    Condition
      Error in `inline_check()`:
      x No in-line functions should be used here.
      i The following function was found: `^`.
      i Use steps to do transformations instead.
      i If your modeling engine uses special terms in formulas, pass that formula to workflows as a model formula (`?parsnip::model_formula()`).

# Using prepare

    Code
      prepare(recipe(HHV ~ ., data = biomass), training = biomass)
    Condition
      Error in `prepare()`:
      ! As of version 0.0.1.9006 please use `prep()` instead of `prepare()`.

# bake without prep

    Code
      bake(sp_signed, new_data = biomass_te)
    Condition
      Error in `bake()`:
      x At least one step has not been trained.
      i Please run `prep()` (`?recipes::prep()`).

---

    Code
      juice(sp_signed)
    Condition
      Error in `juice()`:
      x At least one step has not been trained.
      i Please run `prep()` (`?recipes::prep()`).

# bake without newdata

    Code
      bake(rec, newdata = biomass)
    Condition
      Error in `bake()`:
      ! `new_data` must be either a data frame or NULL. No value is not allowed.

# tunable arguments at prep-time

    Code
      recipe(Species ~ ., data = iris) %>% step_ns(all_predictors(), deg_free = .tune()) %>%
        prep()
    Condition
      Error in `prep()`:
      x You cannot `prep()` a tunable recipe.
      i The following step has `tune()`:
      * step_ns: `deg_free`

---

    Code
      recipe(~., data = mtcars) %>% step_pca(all_predictors(), threshold = .tune()) %>%
        step_kpca(all_predictors(), num_comp = .tune()) %>% step_bs(all_predictors(),
      deg_free = .tune()) %>% prep()
    Condition
      Error in `prep()`:
      x You cannot `prep()` a tunable recipe.
      i The following steps have `tune()`:
      * step_pca: `threshold`
      * step_kpca: `num_comp`
      * step_bs: `deg_free`

# logging

    Code
      recipe(mpg ~ ., data = mtcars) %>% step_ns(disp, deg_free = 2, id = "splines!") %>%
        prep(log_changes = TRUE)
    Output
      step_ns (splines!): 
       new (2): disp_ns_1, disp_ns_2
       removed (1): disp
      
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 10
      
      -- Training information 
      Training data contained 32 data points and no incomplete rows.
      
      -- Operations 
      * Natural splines on: disp | Trained

# case weights are being infered correctly for formula interface

    Code
      recipe(mpg ~ cyl + disp, data = mtcars2)
    Condition
      Error in `recipe()`:
      ! There should only be a single column with the role `case_weights`.
      i In these data, there are 2 columns: `cyl` and `disp`.

# case weights are being infered correctly for x interface

    Code
      recipe(mtcars2)
    Condition
      Error in `recipe()`:
      ! There should only be a single column with the role `case_weights`.
      i In these data, there are 2 columns: `cyl` and `disp`.

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
      x `bake()` methods should always return tibbles.
      i `bake.step_testthat_helper()` returned a data frame.

---

    Code
      prep(rec_spec)
    Condition
      Error in `prep()`:
      x `bake()` methods should always return tibbles.
      i `bake.step_testthat_helper()` returned a data frame.

# recipe() errors if `data` is missing

    Code
      recipe(mpg ~ .)
    Condition
      Error in `recipe()`:
      ! Argument `data` is missing, with no default.

