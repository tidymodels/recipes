library(testthat)
library(tibble)
library(recipes)

context("Testing basic functionalities")

library(modeldata)
data(biomass)
biomass_tr <- biomass[biomass$dataset == "Training",]
biomass_te <- biomass[biomass$dataset == "Testing",]

test_that("Recipe correctly identifies output variable", {
  raw_recipe <- recipe(HHV ~ ., data = biomass)
  var_info <- raw_recipe$var_info
  expect_true(is_tibble(var_info))
  outcome_ind <- which(var_info$variable == "HHV")
  expect_true(var_info$role[outcome_ind] == "outcome")
  expect_true(all(var_info$role[-outcome_ind] == rep("predictor", ncol(biomass) - 1)))
})

test_that("Recipe fails on in-line functions", {
  expect_error(recipe(HHV ~ log(nitrogen), data = biomass))
  expect_error(recipe(HHV ~ (.)^2, data = biomass))
  expect_error(recipe(HHV ~ nitrogen  + sulfur  + nitrogen:sulfur, data = biomass))
  expect_error(recipe(HHV ~ nitrogen^2, data = biomass))
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
  expect_error(prepare(recipe(HHV ~ ., data = biomass),
                       training = biomass),
               paste0("As of version 0.0.1.9006, used `prep` ",
                      "instead of `prepare`"))
})

test_that("Multiple variables on lhs of formula", {
  # from issue #96
  expect_silent(multi_1 <- recipe(Petal.Width + Species ~ ., data = iris))
  expect_equal(multi_1$var_info$variable[multi_1$var_info$role == "outcome"],
               names(iris)[4:5])
  expect_equal(multi_1$var_info$variable[multi_1$var_info$role == "predictor"],
               names(iris)[1:3])

  iris$Species <- as.character(iris$Species)
  expect_silent(multi_2 <- recipe(Petal.Width + Species ~ ., data = iris))
  expect_equal(multi_2$var_info$variable[multi_2$var_info$role == "outcome"],
               names(iris)[4:5])
  expect_equal(multi_2$var_info$variable[multi_2$var_info$role == "predictor"],
               names(iris)[1:3])

})

test_that("detect_step function works", {

  rec <- recipe(Species ~ ., data = iris) %>%
    step_center(all_predictors()) %>%
    step_scale(Sepal.Width) %>%
    step_relu(Sepal.Length) %>%
    step_intercept()

  prepped_rec <- prep(rec, iris)

  # only allow checking for valid steps
  expect_error(detect_step(rec, "not_a_step"))
  expect_error(detect_step(prepped_rec, "not_a_step"))

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
  sp_signed <-  recipe(HHV ~ ., data = biomass) %>%
    step_center(all_predictors()) %>%
    step_scale(all_predictors()) %>%
    step_spatialsign(all_predictors())
  expect_error(
    bake(sp_signed, new_data = biomass_te),
    "At least one step has not been trained. Please run."
  )
  expect_error(
    juice(sp_signed),
    "At least one step has not been trained. Please run."
  )
})


test_that("bake without newdata", {
  rec <-  recipe(HHV ~ ., data = biomass) %>%
    step_center(all_numeric()) %>%
    step_scale(all_numeric()) %>%
    prep(training = biomass)

  expect_error(bake(rec, newdata = biomass))
})

test_that("`juice()` returns a 0 column / N row tibble when a selection returns no columns", {
  rec <- recipe(~ ., data = iris)
  rec <- prep(rec, iris)

  expect_equal(
    juice(rec, all_outcomes()),
    tibble(.rows = nrow(iris))
  )
})

test_that("`bake()` returns a 0 column / N row tibble when a selection returns no columns", {
  rec <- recipe(~ ., data = iris)
  rec <- prep(rec, iris)

  expect_equal(
    bake(rec, iris, all_outcomes()),
    tibble(.rows = nrow(iris))
  )
})

test_that("tunable arguments at prep-time", {
 .tune <- function() rlang::call2("tune")

 expect_error(
   recipe(Species ~ ., data = iris) %>%
     step_ns(all_predictors(), deg_free = .tune()) %>%
     prep(),
   "'deg_free'. Do you want "
 )
})

test_that("logging", {

  expect_output(
    recipe(mpg ~ ., data = mtcars) %>%
      step_ns(disp, deg_free = 2, id = "splines!") %>%
      prep(log_changes = TRUE),
    "splines"
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
