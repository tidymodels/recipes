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
      prep(recipe(~., data = mtcars), fresh = TRUE)
    Condition
      Error in `prep()`:
      ! A training set must be supplied to the `training` argument when `fresh = TRUE`.

---

    Code
      prep(recipe(~., data = mtcars), mtcars[, 1:2], fresh = TRUE)
    Condition
      Error in `prep()`:
      ! Not all variables in the recipe are present in the supplied training set: `disp`, `hp`, `drat`, `wt`, `qsec`, `vs`, `am`, `gear`, and `carb`.

---

    Code
      prep(prep(step_center(recipe(~., data = mtcars), disp), retain = FALSE), mtcars,
      fresh = FALSE)
    Condition
      Error in `prep()`:
      ! To prep new steps after prepping the original recipe, `retain = TRUE` must be set each time that the recipe is trained.

---

    Code
      tmp <- prep(prep(step_center(recipe(~., data = mtcars), disp)), mtcars)
    Condition
      Warning in `prep()`:
      ! The previous data will be used by `prep()`.
      i The data passed using `training` will be ignored.

# spline error messages

    Code
      prep(step_spline_convex(recipe(. ~ disp, data = mtcars), disp))
    Condition
      Error in `step_spline_convex()`:
      Caused by error in `spline2_create()`:
      ! Failed to compute:
      Caused by error in `splines2::cSpline()`:
      ! mocked error

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

# check_options() works

    Code
      check_options(c("unname", "arguments"))
    Condition
      Error:
      ! `options` must be a list, not a character vector.

---

    Code
      check_options(list("unname", "arguments"))
    Condition
      Error:
      ! The list passed to `options` must be named.

---

    Code
      check_options(list(a = 1, b = 2), exclude = "b")
    Condition
      Error:
      ! The following elements of the list passed to `options` are not allowed: a and b.

---

    Code
      check_options(list(a = 1, b = 2), include = "b")
    Condition
      Error:
      ! `options` must only contain elements b, the following are not allowed: a.

# recipes_argument_select() works with single selection

    Code
      helper(NULL)
    Condition
      Error in `helper()`:
      ! `outcome` must not be `NULL`.

---

    Code
      helper(not_mpg)
    Condition
      Error in `helper()`:
      ! Can't select columns that don't exist.
      x Column `not_mpg` doesn't exist.

---

    Code
      helper(c())
    Condition
      Error in `helper()`:
      ! only 1 selection is allowed in `outcome`, not 0.

---

    Code
      helper(vars())
    Condition
      Error in `helper()`:
      x only 1 selection is allowed in `outcome`, not 0.
      i For this argument consider using bare names instead.

---

    Code
      helper(imp_vars())
    Condition
      Error in `helper()`:
      x only 1 selection is allowed in `outcome`, not 0.
      i For this argument consider using bare names instead.

---

    Code
      helper(c(mpg, disp))
    Condition
      Error in `helper()`:
      ! only 1 selection is allowed in `outcome`, not 2.

---

    Code
      helper(c("mpg", "disp"))
    Condition
      Error in `helper()`:
      ! only 1 selection is allowed in `outcome`, not 2.

---

    Code
      helper(vars(mpg, disp))
    Condition
      Error in `helper()`:
      x only 1 selection is allowed in `outcome`, not 2.
      i For this argument consider using bare names instead.

---

    Code
      helper(imp_vars(mpg, disp))
    Condition
      Error in `helper()`:
      x only 1 selection is allowed in `outcome`, not 2.
      i For this argument consider using bare names instead.

# recipes_argument_select() works with multiple selections

    Code
      helper(NULL)
    Condition
      Error in `helper()`:
      ! `outcome` must not be `NULL`.

---

    Code
      helper(not_mpg)
    Condition
      Error in `helper()`:
      ! Can't select columns that don't exist.
      x Column `not_mpg` doesn't exist.

---

    Code
      helper(c())
    Condition
      Error in `helper()`:
      ! only 1 selection is allowed in `outcome`, not 0.

---

    Code
      helper(vars())
    Condition
      Error in `helper()`:
      ! only 1 selection is allowed in `outcome`, not 0.

---

    Code
      helper(imp_vars())
    Condition
      Error in `helper()`:
      ! only 1 selection is allowed in `outcome`, not 0.

# recipes_argument_select() errors on case_weights

    Code
      helper(gear)
    Condition
      Error in `helper()`:
      ! Cannot select case weights variable for `outcome`.

---

    Code
      helper(gear)
    Condition
      Error in `helper()`:
      ! Cannot select case weights variable for `outcome`.

---

    Code
      helper(vars(gear))
    Condition
      Error in `helper()`:
      ! Cannot select case weights variable for `outcome`.

---

    Code
      helper(imp_vars(gear))
    Condition
      Error in `helper()`:
      ! Cannot select case weights variable for `outcome`.

---

    Code
      helper(starts_with("gea"))
    Condition
      Error in `helper()`:
      ! Cannot select case weights variable for `outcome`.

---

    Code
      helper(c(mpg, gear))
    Condition
      Error in `helper()`:
      ! Cannot select case weights variable for `outcome`.

---

    Code
      helper(c("mpg", "gear"))
    Condition
      Error in `helper()`:
      ! Cannot select case weights variable for `outcome`.

---

    Code
      helper(vars(mpg, gear))
    Condition
      Error in `helper()`:
      ! Cannot select case weights variable for `outcome`.

---

    Code
      helper(imp_vars(mpg, gear))
    Condition
      Error in `helper()`:
      ! Cannot select case weights variable for `outcome`.

