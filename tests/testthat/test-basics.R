library(testthat)
library(recipes)

skip_if_not_installed("modeldata")
data(biomass, package = "modeldata")
biomass_tr <- biomass[biomass$dataset == "Training", ]
biomass_te <- biomass[biomass$dataset == "Testing", ]

test_that("Recipe correctly identifies output variable", {
  raw_recipe <- recipe(HHV ~ ., data = biomass)
  var_info <- raw_recipe$var_info
  expect_true(is_tibble(var_info))
  outcome_ind <- which(var_info$variable == "HHV")
  expect_true(var_info$role[outcome_ind] == "outcome")
  expect_true(all(var_info$role[-outcome_ind] == rep("predictor", ncol(biomass) - 1)))
})

test_that("Recipe fails on in-line functions", {
  expect_snapshot(error = TRUE,
                  recipe(HHV ~ log(nitrogen), data = biomass)
  )
  expect_snapshot(error = TRUE,
                  recipe(HHV ~ (.)^2, data = biomass)
  )
  expect_snapshot(error = TRUE,
                  recipe(HHV ~ nitrogen + sulfur + nitrogen:sulfur, data = biomass)
  )
  expect_snapshot(error = TRUE,
                  recipe(HHV ~ nitrogen^2, data = biomass)
  )
})

test_that("Recipe on missspelled variables in formulas", {
  expect_snapshot(
    error = TRUE,
    recipe(HHV ~ not_nitrogen, data = biomass)
  )

  expect_snapshot(
    error = TRUE,
    recipe(not_HHV ~ nitrogen, data = biomass)
  )
})

test_that("return character or factor values", {
  raw_recipe <- recipe(HHV ~ ., data = biomass)
  centered <- raw_recipe %>%
    step_center(carbon, hydrogen, oxygen, nitrogen, sulfur)

  centered_char <- prep(centered, training = biomass, strings_as_factors = FALSE)
  char_var <- bake(centered_char, new_data = head(biomass))
  expect_equal(class(char_var$sample), "character")

  centered_fac <- prep(centered, training = biomass, strings_as_factors = TRUE)
  fac_var <- bake(centered_fac, new_data = head(biomass))
  expect_equal(class(fac_var$sample), "factor")
  expect_equal(levels(fac_var$sample), sort(unique(biomass$sample)))
})


test_that("Using prepare", {
  expect_snapshot(error = TRUE,
                  prepare(recipe(HHV ~ ., data = biomass),
                          training = biomass
                  )
  )
})

test_that("Multiple variables on lhs of formula", {
  # from issue #96
  expect_silent(multi_1 <- recipe(Petal.Width + Species ~ ., data = iris))
  expect_equal(
    multi_1$var_info$variable[multi_1$var_info$role == "outcome"],
    names(iris)[4:5]
  )
  expect_equal(
    multi_1$var_info$variable[multi_1$var_info$role == "predictor"],
    names(iris)[1:3]
  )

  iris$Species <- as.character(iris$Species)
  expect_silent(multi_2 <- recipe(Petal.Width + Species ~ ., data = iris))
  expect_equal(
    multi_2$var_info$variable[multi_2$var_info$role == "outcome"],
    names(iris)[4:5]
  )
  expect_equal(
    multi_2$var_info$variable[multi_2$var_info$role == "predictor"],
    names(iris)[1:3]
  )
})

test_that("detect_step function works", {
  rec <- recipe(Species ~ ., data = iris) %>%
    step_center(all_predictors()) %>%
    step_scale(Sepal.Width) %>%
    step_relu(Sepal.Length) %>%
    step_intercept()

  prepped_rec <- prep(rec, iris)

  # detect untrained steps
  expect_true(detect_step(rec, "center"))
  expect_true(detect_step(rec, "scale"))
  expect_true(detect_step(rec, "relu"))
  expect_true(detect_step(rec, "intercept"))

  # detect trained steps
  expect_true(detect_step(prepped_rec, "center"))
  expect_true(detect_step(prepped_rec, "scale"))
  expect_true(detect_step(prepped_rec, "relu"))
  expect_true(detect_step(prepped_rec, "intercept"))

  # don't detect untrained steps not in use
  expect_false(detect_step(rec, "pca"))
  expect_false(detect_step(rec, "meanimpute"))

  # don't detect trained steps not in use
  expect_false(detect_step(prepped_rec, "pca"))
  expect_false(detect_step(prepped_rec, "meanimpute"))
})

test_that("bake without prep", {
  sp_signed <- recipe(HHV ~ ., data = biomass) %>%
    step_center(all_predictors()) %>%
    step_scale(all_predictors()) %>%
    step_spatialsign(all_predictors())
  expect_snapshot(error = TRUE,
                  bake(sp_signed, new_data = biomass_te)
  )
  expect_snapshot(error = TRUE,
                  juice(sp_signed)
  )
})

test_that("prep with fresh = TRUE", {
  test_data <- data.frame(x = factor(c(1, 2), levels = 1:2), y = c(1, 2))

  rec <-
    recipe(y ~ ., data = test_data) %>%
    step_dummy(x, id = "") %>%
    prep()

  new_rec <- prep(rec, training = test_data, fresh = TRUE)

  rec$fit_times$elapsed <- 0
  new_rec$fit_times$elapsed <- 0

  expect_identical(rec, new_rec)

  expect_equal(
    tidy(new_rec, 1),
    tibble(terms = "x", columns = "2", id = "")
  )

  test_data2 <- data.frame(x = factor(c(2, 1), levels = 1:2), y = c(1, 2))

  new_rec2 <- prep(rec, training = test_data2, fresh = TRUE)

  expect_equal(
    tidy(new_rec2, 1),
    tibble(terms = "x", columns = "2", id = "")
  )
})

test_that("bake without newdata", {
  rec <- recipe(HHV ~ ., data = biomass) %>%
    step_center(all_numeric()) %>%
    step_scale(all_numeric()) %>%
    prep(training = biomass)

  expect_snapshot(error = TRUE,
                  bake(rec, newdata = biomass)
  )
})

test_that("`juice()` returns a 0 column / N row tibble when a selection returns no columns", {
  rec <- recipe(~., data = iris)
  rec <- prep(rec, iris)

  expect_equal(
    juice(rec, all_outcomes()),
    tibble(.rows = nrow(iris))
  )
})

test_that("`bake()` returns a 0 column / N row tibble when a selection returns no columns", {
  rec <- recipe(~., data = iris)
  rec <- prep(rec, iris)

  expect_equal(
    bake(rec, iris, all_outcomes()),
    tibble(.rows = nrow(iris))
  )
})

test_that("tunable arguments at prep-time", {
  .tune <- function() rlang::call2("tune")

  expect_snapshot(error = TRUE,
                  recipe(Species ~ ., data = iris) %>%
                    step_ns(all_predictors(), deg_free = .tune()) %>%
                    prep()
  )

  expect_snapshot(error = TRUE,
                  recipe(~., data = mtcars) %>%
                    step_pca(all_predictors(), threshold = .tune()) %>%
                    step_kpca(all_predictors(), num_comp = .tune()) %>%
                    step_bs(all_predictors(), deg_free = .tune()) %>%
                    prep()
  )
})

test_that("logging", {
  expect_snapshot(
    recipe(mpg ~ ., data = mtcars) %>%
      step_ns(disp, deg_free = 2, id = "splines!") %>%
      prep(log_changes = TRUE)
  )
})

test_that("`bake(new_data = NULL)` same as `juice()`", {
  rec <-
    recipe(mpg ~ ., data = mtcars) %>%
    step_filter(gear == 4) %>%
    step_center(all_predictors()) %>%
    prep()

  juiced <- juice(rec)
  baked <- bake(rec, new_data = NULL)
  expect_equal(juiced, baked)

  # make sure that filter is skipped on training data this way
  roasted <- bake(rec, new_data = mtcars)
  expect_equal(nrow(roasted), 32)
})

test_that("`retain flag in prep should return data when TRUE and zero rows when FALSE", {
  rec <- recipe(mpg ~ ., data = mtcars %>% head(20))

  # flag TRUE but no training data
  prec_1 <- prep(rec, retain = TRUE)
  expect_equal(nrow(prec_1$template), 20)

  # flag FALSE and no training data
  prec_2 <- prep(rec, retain = FALSE)
  expect_equal(nrow(prec_2$template), 0)

  # flag TRUE with training data
  prec_3 <- prep(rec, training = mtcars %>% tail(12), retain = TRUE)
  expect_equal(nrow(prec_3$template), 12)

  # flag FALSE with training data
  prec_4 <- prep(rec, training = mtcars %>% tail(12), retain = FALSE)
  expect_equal(nrow(prec_4$template), 0)
})

test_that("case weights are being infered correctly for formula interface", {
  mtcars1 <- mtcars
  mtcars1$disp <- importance_weights(mtcars1$disp)

  rec <- recipe(mpg ~ cyl + disp, data = mtcars1)

  ref_summary <- tibble(
    variable = c("cyl", "disp", "mpg"),
    type = list(c("double", "numeric"), "case_weights", c("double", "numeric")),
    role = c("predictor", "case_weights", "outcome"),
    source = "original"
  )
  expect_equal(
    summary(rec),
    ref_summary
  )

  mtcars2 <- mtcars
  mtcars2$disp <- importance_weights(mtcars2$disp)
  mtcars2$cyl <- importance_weights(mtcars2$cyl)

  expect_snapshot(error = TRUE,
                  recipe(mpg ~ cyl + disp, data = mtcars2)
  )
})

test_that("case weights are being infered correctly for x interface", {
  mtcars1 <- mtcars[c(2, 3, 1)]
  mtcars1$disp <- importance_weights(mtcars1$disp)

  rec <- recipe(mtcars1)

  ref_summary <- tibble(
    variable = c("cyl", "disp", "mpg"),
    type = list(c("double", "numeric"), "case_weights", c("double", "numeric")),
    role = c(NA, "case_weights", NA),
    source = "original"
  )
  expect_equal(
    summary(rec),
    ref_summary
  )

  mtcars2 <- mtcars
  mtcars2$disp <- importance_weights(mtcars2$disp)
  mtcars2$cyl <- importance_weights(mtcars2$cyl)

  expect_snapshot(error = TRUE,
                  recipe(mtcars2)
  )

})


test_that("verbose when printing", {
  standardized <- recipe(~., mtcars) %>%
    step_center(all_predictors()) %>%
    step_scale(all_predictors()) %>%
    step_normalize(all_predictors())
  expect_snapshot(tmp <- prep(standardized, verbose = TRUE))

})

test_that("`internal data is kept as tibbles when prepping", {
  rec_spec <- recipe(mpg ~ ., data = mtcars) %>%
    step_testthat_helper()

  expect_true(
    tibble::is_tibble(prep(rec_spec)$template)
  )

  expect_true(
    tibble::is_tibble(rec_spec %>% prep() %>% bake(new_data = NULL))
  )

  expect_true(
    tibble::is_tibble(rec_spec %>% prep() %>% bake(new_data = mtcars))
  )

  rec_prepped <- prep(rec_spec)

  # Pretending that the outcome will be a data.frame
  rec_prepped$steps[[1]]$output <- mtcars

  expect_true(
    tibble::is_tibble(bake(rec_prepped, new_data = NULL))
  )

  # Will ignore new_data and return `output`
  expect_snapshot(error = TRUE,
                  bake(rec_prepped, new_data = as_tibble(mtcars))
  )

  rec_spec <- recipe(mpg ~ ., data = mtcars) %>%
    step_testthat_helper(output = mtcars)

  expect_snapshot(error = TRUE, prep(rec_spec))
})

test_that("recipe() errors if `data` is missing", {
  expect_snapshot(error = TRUE, recipe(mpg ~ .))
})

test_that("NAs aren't dropped in strings2factor() (#1291)", {
  ex_data <- tibble(
    x = factor(c("a", NA, "c"), exclude = NULL)
  )

  rec_res <- recipe(~., data = ex_data) %>%
    prep() %>%
    bake(new_data = NULL)

  expect_identical(rec_res, ex_data)
})

test_that("recipe() can handle very long formulas (#1283)", {
  df <- matrix(1:10000, ncol = 10000)
  df <- as.data.frame(df)
  names(df) <- c(paste0("x", 1:10000))

  long_formula <- as.formula(paste("~ ", paste(names(df), collapse = " + ")))

  expect_no_error(
    rec <- recipe(long_formula, df)
  )
})

test_that("recipe() works with odd formula usage (#1283)", {

  expect_identical(
    sort(recipe(mpg ~ ., data = mtcars)$var_info$variable),
    sort(colnames(mtcars))
  )

  expect_identical(
    sort(recipe(mpg ~ . + disp, data = mtcars)$var_info$variable),
    sort(colnames(mtcars))
  )

  expect_identical(
    sort(recipe(mpg ~ disp + disp, mtcars)$var_info$variable),
    c("disp", "mpg")
  )
})

test_that("steps give errors when arguments are misspelled", {
  expect_snapshot(
    error = TRUE,
    recipe(mpg ~ ., data = mtcars) %>%
      step_pca(vs, am, gear, number = 2) %>%
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(mpg ~ ., data = mtcars) %>%
      step_normalize(vs, AM = am, GEAR = gear) %>%
      prep()
  )
})

test_that("data argument is checked in recipe.formula() (#1325)", {
  expect_snapshot(
    error = TRUE,
    recipe(~ a, data = data)
  )
  expect_snapshot(
    error = TRUE,
    recipe(~ ., data = data)
  )
})

test_that("bake() error on wrong composition", {
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) %>%
      prep() %>%
      bake(mtcars, composition = "wrong")
  )
})

test_that("juice() error on wrong composition", {
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) %>%
      prep() %>%
      juice(composition = "wrong")
  )
})

test_that("juice() error if prep(retain = FALSE)", {
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) %>%
      prep(retain = FALSE) %>%
      juice()
  )
})

test_that("recipe() error with minus in formula", {
  expect_snapshot(
    error = TRUE,
    recipe(~ . - 1, data = mtcars)
  )
})

test_that("recipe() error if vars and roles have different lengths", {
  expect_snapshot(
    error = TRUE,
    recipe(mtcars, vars = c("mpg", "disp"), roles = c("predictor"))
  )
})

test_that("recipe() error if vars not in data", {
  expect_snapshot(
    error = TRUE,
    recipe(mtcars, vars = c("wrong", "disp-wrong"))
  )
})

test_that("recipe() error if vars contains duplicates", {
  expect_snapshot(
    error = TRUE,
    recipe(mtcars, vars = c("mpg", "mpg"))
  )
})

test_that("recipe() error if vars and roles are used with formula", {
  expect_snapshot(
    error = TRUE,
    recipe(mtcars, ~., vars = c("mpg"))
  )
  expect_snapshot(
    error = TRUE,
    recipe(mtcars, ~., roles = c("mpg"))
  )
})

test_that("recipe() error for unsupported data types", {
  expect_snapshot(
    error = TRUE,
    recipe(list())
  )
})
