# check_new_data works

    Code
      bake(log_obj, examples[, 2:4, drop = FALSE])
    Condition
      Error in `step_log()`:
      ! The following required column is missing from `new_data` in step 'log_IhS7o': V1.

---

    Code
      bake(log_obj, examples[, 3:4, drop = FALSE])
    Condition
      Error in `step_log()`:
      ! The following required columns are missing from `new_data` in step 'log_IhS7o': V1 and V2.

---

    Code
      bake(log_obj, examples[, 4, drop = FALSE])
    Condition
      Error in `step_log()`:
      ! The following required columns are missing from `new_data` in step 'log_IhS7o': V1, V2, and V3.

# conditionMessage method for recipes errors works

    Code
      conditionMessage(attr(res, "condition"))
    Output
      [1] "Error in `step_dummy()`:\nCaused by error in `prep()`:\nx All columns selected for the step should be factor or ordered.\n* 11 double variables found: `mpg`, `cyl`, `disp`, `hp`, ..."

# check_training_set errors are thrown

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

