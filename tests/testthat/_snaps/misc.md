# check_new_data works

    Code
      bake(log_obj, examples[, 2:4, drop = FALSE])
    Condition
      Error in `step_log()`:
      ! The following required column is missing from `new_data`: V1.

---

    Code
      bake(log_obj, examples[, 3:4, drop = FALSE])
    Condition
      Error in `step_log()`:
      ! The following required columns are missing from `new_data`: V1 and V2.

---

    Code
      bake(log_obj, examples[, 4, drop = FALSE])
    Condition
      Error in `step_log()`:
      ! The following required columns are missing from `new_data`: V1, V2, and V3.

# conditionMessage method for recipes errors works

    Code
      conditionMessage(attr(res, "condition"))
    Output
      [1] "Error in `step_dummy()`:\nCaused by error in `prep()`:\nx All columns selected for the step should be factor or ordered.\n* 11 double variables found: `mpg`, `cyl`, `disp`, `hp`, ..."

# validate_training_data errors are thrown

    Code
      recipe(~., data = mtcars) %>% prep(fresh = TRUE)
    Condition
      Error in `prep()`:
      ! A training set must be supplied to the `training` argument when `fresh = TRUE`.

---

    Code
      recipe(~., data = mtcars) %>% prep(mtcars[, 1:2], fresh = TRUE)
    Condition
      Error in `prep()`:
      ! Not all variables in the recipe are present in the supplied training set: `disp`, `hp`, `drat`, `wt`, `qsec`, `vs`, `am`, `gear`, and `carb`.

---

    Code
      recipe(~., data = mtcars) %>% step_center(disp) %>% prep(retain = FALSE) %>%
        prep(mtcars, fresh = FALSE)
    Condition
      Error in `prep()`:
      ! To prep new steps after prepping the original recipe, `retain = TRUE` must be set each time that the recipe is trained.

---

    Code
      tmp <- recipe(~., data = mtcars) %>% step_center(disp) %>% prep() %>% prep(
        mtcars)
    Condition
      Warning in `prep()`:
      ! The previous data will be used by `prep()`.
      i The data passed using `training` will be ignored.

# spline error messages

    Code
      recipes:::spline_msg("Error in if (df < 0) { : missing blah blah\n")
    Condition
      Error in `recipes:::spline_msg()`:
      ! Error in if (df < 0) { : missing blah blah

---

    Code
      recipes:::spline_msg("craaazzyy {{}}{}{}")
    Condition
      Error in `recipes:::spline_msg()`:
      ! craaazzyy {{}}{}{}

# names0() error on non-positive number

    Code
      names0(0)
    Condition
      Error:
      ! `num` must be a whole number larger than or equal to 1, not the number 0.

# ellipse_check() errors on empty selection

    Code
      ellipse_check()
    Condition
      Error in `ellipse_check()`:
      ! Please supply at least one variable specification.
      i See ?selections (`?recipes::selections()`) for more information.

---

    Code
      uses_dim_red(x)
    Condition
      Error in `uses_dim_red()`:
      ! Recipes version >= 0.1.17 represents the estimates using a different format. Please recreate this recipe or use version 0.1.16 or less. See issue #823 (<https://github.com/tidymodels/recipes/issues/823>).

