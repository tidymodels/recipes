library(testthat)
context("Testing basic functionalities")
library(tibble)

library(recipes)
data("biomass")

test_that("Recipe correctly identifies output variable", {
  raw_recipe <- recipe(HHV ~ ., data = biomass)
  var_info <- raw_recipe$var_info
  expect_true(is.tibble(var_info))
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

  centered_char <- prep(centered, training = biomass, stringsAsFactors = FALSE, retain = TRUE)
  char_var <- bake(centered_char, newdata = head(biomass))
  expect_equal(class(char_var$sample), "character")

  centered_fac <- prep(centered, training = biomass, stringsAsFactors = TRUE, retain = TRUE)
  fac_var <- bake(centered_fac, newdata = head(biomass))
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
  expect_true(detect_step(rec, "step_center"))
  expect_true(detect_step(rec, "step_scale"))
  expect_true(detect_step(rec, "step_relu"))
  expect_true(detect_step(rec, "step_intercept"))

  # detect trained steps
  expect_true(detect_step(prepped_rec, "step_center"))
  expect_true(detect_step(prepped_rec, "step_scale"))
  expect_true(detect_step(prepped_rec, "step_relu"))
  expect_true(detect_step(prepped_rec, "step_intercept"))

  # don't detect untrained steps not in use
  expect_false(detect_step(rec, "step_pca"))
  expect_false(detect_step(rec, "step_meanimpute"))

  # don't detect trained steps not in use
  expect_false(detect_step(prepped_rec, "step_pca"))
  expect_false(detect_step(prepped_rec, "step_meanimpute"))
})
