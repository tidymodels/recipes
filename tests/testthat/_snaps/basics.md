# Recipe fails on in-line functions

    Code
      recipe(HHV ~ log(nitrogen), data = biomass)
    Condition
      Error in `recipe()`:
      x Misspelled variable name or in-line functions detected.
      i The following function/misspelling was found: `log`.
      i Use steps to do transformations instead.
      i If your modeling engine uses special terms in formulas, pass that formula to workflows as a model formula (`?parsnip::model_formula()`).

---

    Code
      recipe(HHV ~ (.)^2, data = biomass)
    Condition
      Error in `recipe()`:
      x Misspelled variable name or in-line functions detected.
      i The following functions/misspellings were found: `^` and `(`.
      i Use steps to do transformations instead.
      i If your modeling engine uses special terms in formulas, pass that formula to workflows as a model formula (`?parsnip::model_formula()`).

---

    Code
      recipe(HHV ~ nitrogen + sulfur + nitrogen:sulfur, data = biomass)
    Condition
      Error in `recipe()`:
      x Misspelled variable name or in-line functions detected.
      i The following function/misspelling was found: `:`.
      i Use steps to do transformations instead.
      i If your modeling engine uses special terms in formulas, pass that formula to workflows as a model formula (`?parsnip::model_formula()`).

---

    Code
      recipe(HHV ~ nitrogen^2, data = biomass)
    Condition
      Error in `recipe()`:
      x Misspelled variable name or in-line functions detected.
      i The following function/misspelling was found: `^`.
      i Use steps to do transformations instead.
      i If your modeling engine uses special terms in formulas, pass that formula to workflows as a model formula (`?parsnip::model_formula()`).

# Recipe on missspelled variables in formulas

    Code
      recipe(HHV ~ not_nitrogen, data = biomass)
    Condition
      Error in `recipe()`:
      x Misspelled variable name or in-line functions detected.
      i The following function/misspelling was found: `not_nitrogen`.
      i Use steps to do transformations instead.
      i If your modeling engine uses special terms in formulas, pass that formula to workflows as a model formula (`?parsnip::model_formula()`).

---

    Code
      recipe(not_HHV ~ nitrogen, data = biomass)
    Condition
      Error in `recipe()`:
      x Misspelled variable name or in-line functions detected.
      i The following function/misspelling was found: `not_HHV`.
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
      ! `data` is missing with no default.

# steps give errors when arguments are misspelled

    Code
      recipe(mpg ~ ., data = mtcars) %>% step_pca(vs, am, gear, number = 2) %>% prep()
    Condition
      Error in `step_pca()`:
      Caused by error in `prep()`:
      ! The following argument was specified but does not exist: `number`.

---

    Code
      recipe(mpg ~ ., data = mtcars) %>% step_normalize(vs, AM = am, GEAR = gear) %>%
        prep()
    Condition
      Error in `step_normalize()`:
      Caused by error in `prep()`:
      ! The following arguments were specified but do not exist: `AM` and `GEAR`.

# data argument is checked in recipe.formula() (#1325)

    Code
      recipe(~a, data = data)
    Condition
      Error in `recipe()`:
      ! `data` must be a data frame, matrix, or sparse matrix, not a function.

---

    Code
      recipe(~., data = data)
    Condition
      Error in `recipe()`:
      ! `data` must be a data frame, matrix, or sparse matrix, not a function.

# step constructor

    Code
      recipe(~., mtcars) %>% step_normalize(trained = "yes")
    Condition
      Error in `step_normalize()`:
      ! `trained` must be `TRUE` or `FALSE`, not the string "yes".

---

    Code
      recipe(~., mtcars) %>% step_normalize(id = TRUE)
    Condition
      Error in `step_normalize()`:
      ! `id` must be a single string, not `TRUE`.

---

    Code
      recipe(~., mtcars) %>% step_normalize(skip = "you betcha")
    Condition
      Error in `step_normalize()`:
      ! `skip` must be `TRUE` or `FALSE`, not the string "you betcha".

---

    Code
      recipe(~., mtcars) %>% step_normalize(role = 13)
    Condition
      Error in `step_normalize()`:
      ! `x$role` must be a single string or `NA`, not the number 13.

---

    Code
      recipe(~., mtcars) %>% step_pca(all_predictors(), keep_original_cols = 0)
    Condition
      Error in `step_pca()`:
      ! `keep_original_cols` must be `TRUE` or `FALSE`, not the number 0.

---

    Code
      step(subclass = list())
    Condition
      Error:
      ! `subclass` must be a single string, not an empty list.

---

    Code
      step()
    Condition
      Error:
      ! `subclass` must be a single string, not absent.

# bake() error on wrong composition

    Code
      recipe(~., data = mtcars) %>% prep() %>% bake(mtcars, composition = "wrong")
    Condition
      Error in `bake()`:
      x `composition` cannot be "wrong".
      i Allowed values are "tibble", "dgCMatrix", "matrix", or "data.frame".

# juice() error on wrong composition

    Code
      recipe(~., data = mtcars) %>% prep() %>% juice(composition = "wrong")
    Condition
      Error in `juice()`:
      x `composition` cannot be "wrong".
      i Allowed values are "tibble", "dgCMatrix", "matrix", or "data.frame".

# juice() error if prep(retain = FALSE)

    Code
      recipe(~., data = mtcars) %>% prep(retain = FALSE) %>% juice()
    Condition
      Error in `juice()`:
      ! Use `retain = TRUE` in `prep()` to be able to extract the training set.

# recipe() error with minus in formula

    Code
      recipe(~ . - 1, data = mtcars)
    Condition
      Error in `recipe()`:
      x `-` is not allowed in a recipe formula.
      i Use `step_rm()` (`?recipes::step_rm()`) instead.

# recipe() error if vars and roles have different lengths

    Code
      recipe(mtcars, vars = c("mpg", "disp"), roles = c("predictor"))
    Condition
      Error in `recipe()`:
      x `vars` and `roles` must have same length.
      * `vars` has length 2
      * `roles` has length 1

# recipe() error if vars not in data

    Code
      recipe(mtcars, vars = c("wrong", "disp-wrong"))
    Condition
      Error in `recipe()`:
      x The following elements of `vars` are not found in `x`:
      * wrong and disp-wrong.

# recipe() error if vars contains duplicates

    Code
      recipe(mtcars, vars = c("mpg", "mpg"))
    Condition
      Error in `recipe()`:
      x `vars` must have unique values.
      i The following values were duplicated: mpg.

# recipe() error if vars and roles are used with formula

    Code
      recipe(mtcars, ~., vars = c("mpg"))
    Condition
      Error in `recipe()`:
      ! The `vars` argument will be ignored when a formula is used.

---

    Code
      recipe(mtcars, ~., roles = c("mpg"))
    Condition
      Error in `recipe()`:
      ! The `roles` argument will be ignored when a formula is used.

# recipe() error for unsupported data types

    Code
      recipe(list())
    Condition
      Error in `recipe()`:
      x `x` should be a data frame, matrix, formula, or tibble.
      i `x` is an empty list.

